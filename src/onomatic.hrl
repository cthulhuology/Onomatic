%% Onomatic State engine

-record( request, { method, path, protocol, headers=[], trailers=[], content="" }).
-record( response, { protocol, status, message, headers=[], trailers=[], content="" }).

-record( cache, { cache_table }).

-record( route, { source, destination }).
-record( router, { routing_table }).

-record( acl, { get=[], put=[], delete=[], post=[] }).
-record( permissions, { permission_table }).

-record( frontend, {  server, socket, watcher,  clients=[], config }).
-record( backend, {  server, socket, watcher,  clients=[], config }).
-record( websocket, { watcher, clients=[], config }).

-record( sockets, { listen, port=80, connections=[], max_connections=10000 }).

-record( proxy, { peer_table }).
