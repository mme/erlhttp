/* niftest.c */

#include "http_parser.h"
#include "string.h"
#include <ctype.h>
#include "erl_nif.h"
#include <stdio.h>

static ErlNifResourceType *erlhttp_parser_resource = NULL;


typedef struct {
    ErlNifBinary *bin;
    ERL_NIF_TERM bin_term;
    int retained;
} bin_pointer;

typedef struct {
    ErlNifEnv *env;
    ERL_NIF_TERM buf_term;
    ErlNifBinary *buf_bin;
    ERL_NIF_TERM result;
    int version_done;
    int done;
} parser_data;

void emit(parser_data *data, const char *at, size_t length, const char* name)
{
    size_t pos = ((void *)at) - ((void *)data->buf_bin->data);
    ERL_NIF_TERM term = enif_make_sub_binary(data->env, data->buf_term, pos, length);
    data->result = enif_make_list_cell(data->env, 
                        enif_make_tuple2(data->env, enif_make_atom(data->env, name), term), data->result);
}


void emit_version(http_parser *parser)
{
    parser_data *data = (parser_data *)parser->data;
    
    ERL_NIF_TERM version = enif_make_tuple2(data->env, enif_make_int(data->env, parser->http_major), enif_make_int(data->env, parser->http_minor));
    data->result = enif_make_list_cell(data->env, 
                        enif_make_tuple2(data->env, enif_make_atom(data->env, "version"), version),data->result);
    data->version_done = 1;            
}

#define EMIT_VERSION_ONCE(PARSER) if (!((parser_data *)PARSER->data)->version_done) emit_version(PARSER)

int on_message_begin (http_parser *parser)
{
    parser_data *data = (parser_data *)parser->data;
    
    char *buf;
    // Method
    const char *method = http_method_str(parser->method);
    size_t method_len = strlen(method) + 1;
    buf = malloc(method_len);
    strlcpy(buf, method, method_len);
    char *p = buf;
    for ( ; *p; ++p) *p = tolower(*p);
    data->result = enif_make_list_cell(data->env, 
                        enif_make_tuple2(data->env, enif_make_atom(data->env, "method"), enif_make_atom(data->env, buf)), data->result);
    free(buf);

    buf = NULL;                  
    return 0;
}


int on_url (http_parser *parser, const char *at, size_t length)
{
    emit(parser->data, at, length, "url");   
    return 0;
}

int on_status_complete (http_parser *parser)
{
    return 0;
}

int on_header_field (http_parser *parser, const char *at, size_t length)
{   
    EMIT_VERSION_ONCE(parser);
    emit(parser->data, at, length, "header_field");                        
    return 0;
}

int on_header_value (http_parser *parser, const char *at, size_t length)
{
    emit(parser->data, at, length, "header_value");
    return 0;
}

int on_headers_complete (http_parser *parser)
{
    return 0;
}

int on_body (http_parser *parser, const char *at, size_t length)
{
    EMIT_VERSION_ONCE(parser);
    emit(parser->data, at, length, "body");
    return 0;
}

int on_message_complete (http_parser *parser)
{
    EMIT_VERSION_ONCE(parser);
    parser_data *data = (parser_data *)parser->data;
    data->done = 1;
    http_parser_pause(parser, 1);
    return 0;
}

static http_parser_settings settings = {
    .on_message_begin       = on_message_begin,
    .on_url                 = on_url,
    .on_status_complete     = on_status_complete,
    
    .on_header_field        = on_header_field,
    .on_header_value        = on_header_value,
    .on_headers_complete    = on_headers_complete,
    .on_body                = on_body,
    .on_message_complete    = on_message_complete
};

static ERL_NIF_TERM new_parser_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int parser_type = 0;
    
    if (!enif_get_int(env, argv[0], &parser_type)) {
        return enif_make_badarg(env);
    }
    
    if (parser_type != HTTP_REQUEST && parser_type != HTTP_RESPONSE && parser_type != HTTP_BOTH) {
        return enif_make_badarg(env);
    }
        
    http_parser *parser = enif_alloc_resource(erlhttp_parser_resource, sizeof(http_parser));
    parser_data *data = malloc(sizeof(parser_data));
    memset(data,0,sizeof(parser_data));
    http_parser_init(parser, parser_type);
    parser->data = data;
    
    ERL_NIF_TERM parser_term = enif_make_resource(env, parser);
    enif_release_resource(parser);
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), parser_term);
}

static ERL_NIF_TERM parse_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    http_parser *parser;
    
    if (!enif_get_resource(env, argv[0], erlhttp_parser_resource, (void **)&parser)) {
        return enif_make_badarg(env);
    }
    
    ERL_NIF_TERM buf_term = argv[1];
    
    ErlNifBinary buf;
    if (!enif_inspect_binary(env, buf_term, &buf)) {
        return enif_make_badarg(env);
    }
    
    
    // set up parser data
    parser_data *data = parser->data;    
    data->env = env;
    data->buf_term = argv[1];
    data->buf_bin = &buf;
    data->result = enif_make_list(env, 0);

    int nparsed = http_parser_execute(parser, &settings, (const char *)buf.data, buf.size);
    
    
    if (HTTP_PARSER_ERRNO(parser) != HPE_OK && HTTP_PARSER_ERRNO(parser) != HPE_PAUSED) {
        
        const char *reason_str = http_errno_description(HTTP_PARSER_ERRNO(parser));
        size_t reason_str_len = strlen(reason_str);
        ERL_NIF_TERM reason;
        
        unsigned char *buf = enif_make_new_binary(env, reason_str_len, &reason);
        memcpy(buf, reason_str, reason_str_len);

        return enif_make_tuple2(env, enif_make_atom(env, "error"), reason);
    } else {
        
        if (data->done == 1) {
            data->result = enif_make_list_cell(data->env, enif_make_atom(data->env, "done"), data->result);
        }
        
        printf("%d >> %zd\n", nparsed, buf.size);
        
        ERL_NIF_TERM rest;
        if (nparsed != buf.size) {
            rest = enif_make_sub_binary(env, buf_term, nparsed, buf.size-nparsed);
        } else {
            enif_make_new_binary(env, 0, &rest);
        }
 
        return enif_make_tuple3(env, enif_make_atom(env, "ok"), data->result, rest);
    }
}

static ERL_NIF_TERM is_upgrade(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    http_parser *parser;
    
    if (!enif_get_resource(env, argv[0], erlhttp_parser_resource, (void **)&parser)) {
        return enif_make_badarg(env);
    }
    
    return parser->upgrade ? enif_make_atom(env, "true") : enif_make_atom(env, "false");
}

static ERL_NIF_TERM should_keepalive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    http_parser *parser;
    
    if (!enif_get_resource(env, argv[0], erlhttp_parser_resource, (void **)&parser)) {
        return enif_make_badarg(env);
    }
    
    return http_should_keep_alive(parser) ? enif_make_atom(env, "true") : enif_make_atom(env, "false");
}


void erlhttp_parser_resource_dtor(ErlNifEnv* env, void* obj)
{
    
    http_parser *parser = (http_parser *)obj;
    free (parser->data);
}

static ErlNifFunc nif_funcs[] =
{
    {"new_parser_raw", 1, new_parser_raw},
    {"parse_raw", 2, parse_raw},
    {"should_keepalive", 1, should_keepalive},
    {"is_upgrade", 1, is_upgrade}
};

int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) 
{
    erlhttp_parser_resource = enif_open_resource_type(env, "erlhttp", "erlhttp_parser_resource", &erlhttp_parser_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    return 0;
}

ERL_NIF_INIT(erlhttp,nif_funcs,&load,NULL,NULL,NULL)