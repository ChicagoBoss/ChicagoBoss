-record(rpberrorresp, {
    errmsg = erlang:error({required, errmsg}),
    errcode = erlang:error({required, errcode})
}).

-record(rpbgetclientidresp, {
    client_id = erlang:error({required, client_id})
}).

-record(rpbsetclientidreq, {
    client_id = erlang:error({required, client_id})
}).

-record(rpbgetserverinforesp, {
    node,
    server_version
}).

-record(rpbgetreq, {
    bucket = erlang:error({required, bucket}),
    key = erlang:error({required, key}),
    r,
    pr,
    basic_quorum,
    notfound_ok,
    if_modified,
    head,
    deletedvclock
}).

-record(rpbgetresp, {
    content,
    vclock,
    unchanged
}).

-record(rpbputreq, {
    bucket = erlang:error({required, bucket}),
    key,
    vclock,
    content = erlang:error({required, content}),
    w,
    dw,
    return_body,
    pw,
    if_not_modified,
    if_none_match,
    return_head
}).

-record(rpbputresp, {
    content,
    vclock,
    key
}).

-record(rpbdelreq, {
    bucket = erlang:error({required, bucket}),
    key = erlang:error({required, key}),
    rw,
    vclock
}).

-record(rpblistbucketsresp, {
    buckets
}).

-record(rpblistkeysreq, {
    bucket = erlang:error({required, bucket})
}).

-record(rpblistkeysresp, {
    keys,
    done
}).

-record(rpbgetbucketreq, {
    bucket = erlang:error({required, bucket})
}).

-record(rpbgetbucketresp, {
    props = erlang:error({required, props})
}).

-record(rpbsetbucketreq, {
    bucket = erlang:error({required, bucket}),
    props = erlang:error({required, props})
}).

-record(rpbmapredreq, {
    request = erlang:error({required, request}),
    content_type = erlang:error({required, content_type})
}).

-record(rpbmapredresp, {
    phase,
    response,
    done
}).

-record(rpbcontent, {
    value = erlang:error({required, value}),
    content_type,
    charset,
    content_encoding,
    vtag,
    links,
    last_mod,
    last_mod_usecs,
    usermeta
}).

-record(rpbpair, {
    key = erlang:error({required, key}),
    value
}).

-record(rpblink, {
    bucket,
    key,
    tag
}).

-record(rpbbucketprops, {
    n_val,
    allow_mult
}).

