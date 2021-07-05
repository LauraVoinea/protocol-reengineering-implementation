-record(graph, {graph_ref, name, type}).

-record(edge_data, {event_type, event, pattern, args, guard, code, attributes, comments = []}).
-record(edge, {from, to, edge_data}).

-record(trans, {from, to, data}).
-record(data, {action, var, event, cons = []}).
