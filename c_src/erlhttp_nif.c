#include "http_parser.h"
#include "string.h"
#include "erl_nif.h"

static ErlNifResourceType *http_parser_resource = NULL;

static ERL_NIF_TERM k_atom_ok;
static ERL_NIF_TERM k_atom_error;

static ERL_NIF_TERM k_atom_nil;
static ERL_NIF_TERM k_atom_true;
static ERL_NIF_TERM k_atom_false;

static ERL_NIF_TERM k_atom_version;

static ERL_NIF_TERM k_atom_url;
static ERL_NIF_TERM k_atom_header_field;
static ERL_NIF_TERM k_atom_header_value;
static ERL_NIF_TERM k_atom_body;
static ERL_NIF_TERM k_atom_done;

static ERL_NIF_TERM k_atom_method;

static ERL_NIF_TERM k_atom_method_delete;
static ERL_NIF_TERM k_atom_method_get;
static ERL_NIF_TERM k_atom_method_head;
static ERL_NIF_TERM k_atom_method_post;
static ERL_NIF_TERM k_atom_method_put;
static ERL_NIF_TERM k_atom_method_connect;
static ERL_NIF_TERM k_atom_method_options;
static ERL_NIF_TERM k_atom_method_trace;
static ERL_NIF_TERM k_atom_method_copy;
static ERL_NIF_TERM k_atom_method_lock;
static ERL_NIF_TERM k_atom_method_mkcol;
static ERL_NIF_TERM k_atom_method_move;
static ERL_NIF_TERM k_atom_method_propfind;
static ERL_NIF_TERM k_atom_method_proppatch;
static ERL_NIF_TERM k_atom_method_search;
static ERL_NIF_TERM k_atom_method_unlock;
static ERL_NIF_TERM k_atom_method_report;
static ERL_NIF_TERM k_atom_method_mkactivity;
static ERL_NIF_TERM k_atom_method_checkout;
static ERL_NIF_TERM k_atom_method_merge;
static ERL_NIF_TERM k_atom_method_msearch;
static ERL_NIF_TERM k_atom_method_notify;
static ERL_NIF_TERM k_atom_method_subscribe;
static ERL_NIF_TERM k_atom_method_unsubscribe;
static ERL_NIF_TERM k_atom_method_patch;
static ERL_NIF_TERM k_atom_method_purge;

typedef struct {
    ErlNifEnv *env;
    ERL_NIF_TERM buf_term;
    ErlNifBinary *buf_bin;
    ERL_NIF_TERM result;
    int version_done;
} parser_data;

void emit(parser_data *data, const char *at, size_t length, ERL_NIF_TERM name)
{
    size_t pos = ((void *)at) - ((void *)data->buf_bin->data);
    ERL_NIF_TERM term = enif_make_sub_binary(data->env, data->buf_term, pos, length);
    data->result = enif_make_list_cell(data->env, enif_make_tuple2(data->env, name, term), data->result);
}


void emit_version(http_parser *parser)
{
    parser_data *data = (parser_data *)parser->data;

    ERL_NIF_TERM version = enif_make_tuple2(data->env, enif_make_int(data->env, parser->http_major), enif_make_int(data->env, parser->http_minor));
    data->result = enif_make_list_cell(data->env, enif_make_tuple2(data->env, k_atom_version, version),data->result);
    data->version_done = 1;
}

#define EMIT_VERSION_ONCE(PARSER) if (!((parser_data *)PARSER->data)->version_done) emit_version(PARSER)

int on_message_begin (http_parser *parser)
{
    parser_data *data = (parser_data *)parser->data;

    ERL_NIF_TERM method = k_atom_error;

    switch(parser->method) {
        case HTTP_DELETE:      method = k_atom_method_delete;       break;
        case HTTP_GET:         method = k_atom_method_get;          break;
        case HTTP_HEAD:        method = k_atom_method_head;         break;
        case HTTP_POST:        method = k_atom_method_post;         break;
        case HTTP_PUT:         method = k_atom_method_put;          break;
        case HTTP_CONNECT:     method = k_atom_method_connect;      break;
        case HTTP_OPTIONS:     method = k_atom_method_options;      break;
        case HTTP_TRACE:       method = k_atom_method_trace;        break;
        case HTTP_COPY:        method = k_atom_method_copy;         break;
        case HTTP_LOCK:        method = k_atom_method_lock;         break;
        case HTTP_MKCOL:       method = k_atom_method_mkcol;        break;
        case HTTP_MOVE:        method = k_atom_method_move;         break;
        case HTTP_PROPFIND:    method = k_atom_method_propfind;     break;
        case HTTP_PROPPATCH:   method = k_atom_method_proppatch;    break;
        case HTTP_SEARCH:      method = k_atom_method_search;       break;
        case HTTP_UNLOCK:      method = k_atom_method_unlock;       break;
        case HTTP_REPORT:      method = k_atom_method_report;       break;
        case HTTP_MKACTIVITY:  method = k_atom_method_mkactivity;   break;
        case HTTP_CHECKOUT:    method = k_atom_method_checkout;     break;
        case HTTP_MERGE:       method = k_atom_method_merge;        break;
        case HTTP_MSEARCH:     method = k_atom_method_msearch;      break;
        case HTTP_NOTIFY:      method = k_atom_method_notify;       break;
        case HTTP_SUBSCRIBE:   method = k_atom_method_subscribe;    break;
        case HTTP_UNSUBSCRIBE: method = k_atom_method_unsubscribe;  break;
        case HTTP_PATCH:       method = k_atom_method_patch;        break;
        case HTTP_PURGE:       method = k_atom_method_purge;        break;
    }
    data->result = enif_make_list_cell(data->env, enif_make_tuple2(data->env, k_atom_method, method), data->result);

    return 0;
}


int on_url (http_parser *parser, const char *at, size_t length)
{
    emit(parser->data, at, length, k_atom_url);
    return 0;
}

int on_chunk_complete (http_parser *parser)
{
    return 0;
}

int on_header_field (http_parser *parser, const char *at, size_t length)
{
    EMIT_VERSION_ONCE(parser);
    emit(parser->data, at, length, k_atom_header_field);
    return 0;
}

int on_header_value (http_parser *parser, const char *at, size_t length)
{
    emit(parser->data, at, length, k_atom_header_value);
    return 0;
}

int on_headers_complete (http_parser *parser)
{
    return 0;
}

int on_body (http_parser *parser, const char *at, size_t length)
{
    EMIT_VERSION_ONCE(parser);
    emit(parser->data, at, length, k_atom_body);
    return 0;
}

int on_message_complete (http_parser *parser)
{
    EMIT_VERSION_ONCE(parser);
    parser_data *data = (parser_data *)parser->data;
    data->result = enif_make_list_cell(data->env, k_atom_done, data->result);
    http_parser_pause(parser, 1);
    return 0;
}

static http_parser_settings settings = {
    .on_message_begin       = on_message_begin,
    .on_url                 = on_url,
    .on_chunk_complete      = on_chunk_complete,

    .on_header_field        = on_header_field,
    .on_header_value        = on_header_value,
    .on_headers_complete    = on_headers_complete,
    .on_body                = on_body,
    .on_message_complete    = on_message_complete
};

static ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int parser_type = 0;

    if (!enif_get_int(env, argv[0], &parser_type)) {
        return enif_make_badarg(env);
    }

    if (parser_type != HTTP_REQUEST && parser_type != HTTP_RESPONSE && parser_type != HTTP_BOTH) {
        return enif_make_badarg(env);
    }

    http_parser *parser = enif_alloc_resource(http_parser_resource, sizeof(http_parser));
    parser_data *data = malloc(sizeof(parser_data));
    memset(data,0,sizeof(parser_data));
    http_parser_init(parser, parser_type);
    parser->data = data;

    ERL_NIF_TERM parser_term = enif_make_resource(env, parser);
    enif_release_resource(parser);

    return enif_make_tuple2(env, k_atom_ok, parser_term);
}

static ERL_NIF_TERM parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    http_parser *parser;

    if (!enif_get_resource(env, argv[0], http_parser_resource, (void **)&parser)) {
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

        return enif_make_tuple2(env, k_atom_error, reason);
    } else {

        ERL_NIF_TERM rest;
        if (nparsed != buf.size) {
            rest = enif_make_sub_binary(env, buf_term, nparsed, buf.size-nparsed);
        } else {
            enif_make_new_binary(env, 0, &rest);
        }

        return enif_make_tuple3(env, k_atom_ok, data->result, rest);
    }
}

static ERL_NIF_TERM is_upgrade(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    http_parser *parser;

    if (!enif_get_resource(env, argv[0], http_parser_resource, (void **)&parser)) {
        return enif_make_badarg(env);
    }

    return parser->upgrade ? k_atom_true : k_atom_false;
}

static ERL_NIF_TERM should_keepalive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    http_parser *parser;

    if (!enif_get_resource(env, argv[0], http_parser_resource, (void **)&parser)) {
        return enif_make_badarg(env);
    }

    return http_should_keep_alive(parser) ? k_atom_true : k_atom_false;
}


void http_parser_resource_dtor(ErlNifEnv* env, void* obj)
{
    http_parser *parser = (http_parser *)obj;
    free (parser->data);
}

static ErlNifFunc nif_funcs[] =
{
    {"new_parser_raw", 1, new},
    {"parse_raw", 2, parse},
    {"should_keepalive", 1, should_keepalive},
    {"is_upgrade", 1, is_upgrade}
};

static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    *priv = *old_priv;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv)
{
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    http_parser_resource        = enif_open_resource_type(env, "http_parser", "http_parser_resource", &http_parser_resource_dtor, ERL_NIF_RT_CREATE, NULL);

    k_atom_error                = enif_make_atom(env, "error");
    k_atom_ok                   = enif_make_atom(env, "ok");
    k_atom_version              = enif_make_atom(env, "version");
    k_atom_nil                  = enif_make_atom(env, "nil");
    k_atom_true                 = enif_make_atom(env, "true");
    k_atom_false                = enif_make_atom(env, "false");

    k_atom_version              = enif_make_atom(env, "version");
    k_atom_url                  = enif_make_atom(env, "url");
    k_atom_header_field         = enif_make_atom(env, "header_field");
    k_atom_header_value         = enif_make_atom(env, "header_value");
    k_atom_body                 = enif_make_atom(env, "body");
    k_atom_done                 = enif_make_atom(env, "done");
    k_atom_method               = enif_make_atom(env, "method");

    k_atom_method_delete        = enif_make_atom(env, "delete");
    k_atom_method_get           = enif_make_atom(env, "get");
    k_atom_method_head          = enif_make_atom(env, "head");
    k_atom_method_post          = enif_make_atom(env, "post");
    k_atom_method_put           = enif_make_atom(env, "put");
    k_atom_method_connect       = enif_make_atom(env, "connect");
    k_atom_method_options       = enif_make_atom(env, "options");
    k_atom_method_trace         = enif_make_atom(env, "trace");
    k_atom_method_copy          = enif_make_atom(env, "copy");
    k_atom_method_lock          = enif_make_atom(env, "lock");
    k_atom_method_mkcol         = enif_make_atom(env, "mkcol");
    k_atom_method_move          = enif_make_atom(env, "move");
    k_atom_method_propfind      = enif_make_atom(env, "propfind");
    k_atom_method_proppatch     = enif_make_atom(env, "proppatch");
    k_atom_method_search        = enif_make_atom(env, "search");
    k_atom_method_unlock        = enif_make_atom(env, "unlock");
    k_atom_method_report        = enif_make_atom(env, "report");
    k_atom_method_mkactivity    = enif_make_atom(env, "mkactivity");
    k_atom_method_checkout      = enif_make_atom(env, "checkout");
    k_atom_method_merge         = enif_make_atom(env, "merge");
    k_atom_method_msearch       = enif_make_atom(env, "msearch");
    k_atom_method_notify        = enif_make_atom(env, "notify");
    k_atom_method_subscribe     = enif_make_atom(env, "subscribe");
    k_atom_method_unsubscribe   = enif_make_atom(env, "unsubscribe");
    k_atom_method_patch         = enif_make_atom(env, "patch");
    k_atom_method_purge         = enif_make_atom(env, "purge");

    return 0;
}

ERL_NIF_INIT(erlhttp,nif_funcs,&load,&reload,&upgrade,&unload)
