joint.shapes.logic.Timer = joint.shapes.logic.IO.extend({
    markup: '<g class="rotatable"><g class="scalable"><rect class="body"/></g>'+
           '<circle class="input input1"/>'+
           '<circle  class="input input2"/><circle class="output"/>'+
           '<path class="wire wireIn1"/>'+
           '<path class="wire wireIn2"/>'+
           '<path class="wire wireOut"/><text/></g>',
    defaults: joint.util.deepSupplement({
        type: 'logic.Timer',
        size: { width: 60, height: 40 },
        attrs: {
            '.wire': { stroke: 'black'},
            '.wireIn1': { ref: '.body', 'ref-x': -20, 'ref-y': 0.3,
                          d: 'M 0 0 L 20 0' },
            '.wireIn2': { ref: '.body', 'ref-x': -20, 'ref-y': 0.7,
                          d: 'M 0 0 L 20 0' },
            '.wireOut': { ref: '.body', 'ref-dx': 0, 'ref-y': 0.5,
                          d: 'M 0 0 L 20 0' },
            '.input1': { ref: '.body', 'ref-x': -27, 'ref-y': 0.3,
                         magnet: 'passive', port: 'in1' },
            '.input2': { ref: '.body', 'ref-x': -27, 'ref-y': 0.7,
                         magnet: 'passive', port: 'in2' },
            '.output': { ref: '.body', 'ref-dx': 27, 'ref-y': 0.5,
                         magnet: true, port: 'out1' },
            text: {text: 'start\nreset', 'ref-x': 0, 'text-anchor': 'left'}
        }
    }, joint.shapes.logic.IO.prototype.defaults)
});

joint.shapes.logic.Box11 = joint.shapes.logic.IO.extend({
    markup: '<g class="rotatable"><g class="scalable"><rect class="body"/></g>'+
           '<circle class="input"/>'+
           '<circle class="output"/>'+
           '<path class="wire wireIn"/>'+
           '<path class="wire wireOut"/><text/></g>',
    defaults: joint.util.deepSupplement({
        type: 'logic.Timer',
        size: { width: 60, height: 30 },
        attrs: {
            '.wire': { stroke: 'black'},
            '.wireIn': { ref: '.body', 'ref-x': -20, 'ref-y': 0.5,
                          d: 'M 0 0 L 20 0' },
            '.wireOut': { ref: '.body', 'ref-dx': 0, 'ref-y': 0.5,
                          d: 'M 0 0 L 20 0' },
            '.input': { ref: '.body', 'ref-x': -27, 'ref-y': 0.5,
                        magnet: 'passive', port: 'in1' },
            '.output': { ref: '.body', 'ref-dx': 27, 'ref-y': 0.5,
                         magnet: true, port: 'out1' },
            text: {'text-transform': 'none'}
        }
    }, joint.shapes.logic.IO.prototype.defaults)
});

function createEdgeClass(type, text) {
    joint.shapes.logic[type] = joint.shapes.logic.Box11.extend({
        defaults: joint.util.deepSupplement({
            type: 'logic.' + type,
            attrs: {
                text: {text: text}
            }
        }, joint.shapes.logic.Box11.prototype.defaults)
    })
}

createEdgeClass('UpEdge', 'up edge');
createEdgeClass('DownEdge', 'down edge');
createEdgeClass('BothEdge', 'both edge');

var TYPE2SHAPE = {
    'switch': joint.shapes.logic.Input,
    relay: joint.shapes.logic.Output,
    or: joint.shapes.logic.Or,
    and: joint.shapes.logic.And,
    xor: joint.shapes.logic.Xor,
    not: joint.shapes.logic.Not,
    timer: joint.shapes.logic.Timer,
    up_edge: joint.shapes.logic.UpEdge,
    down_edge: joint.shapes.logic.DownEdge,
    both_edge: joint.shapes.logic.BothEdge
};

function graphId(id) {
    return "schema-element-" + id;
}

function parseId(id) {
    return parseInt(id.match(/schema-element-(\d+)/)[1]);
}

function parsePort(port) {
    return parseInt(port.match(/(?:in|out)(\d+)/)[1]);
}

function createCell(model) {
    return new TYPE2SHAPE[model.get('type')]({
        id: graphId(model.get('id')),
        position: {x: model.get('x'), y: model.get('y')},
        element: model
    });
}

function loadGraph(graph, schema) {
    if(!schema) {
        return;
    }

    graph.elementStore = Ext.create('Ext.data.Store', {
        model: 'Element',
        proxy: {
            type: 'rest',
            url: "/schemas/" + schema.getId() + '/elements',
            reader: {
                type: 'json'
            },
            writer: {
                type: 'json',
                writeRecordId: false,
                writeAllFields: true
            }
        },
        autoSync: false
    });

    graph.connectionStore = Ext.create('Ext.data.Store', {
        model: 'Connection',
        proxy: {
            type: 'rest',
            url: "/schemas/" + schema.getId() + '/connections',
            reader: {
                type: 'json'
            },
            writer: {
                type: 'json',
                writeRecordId: false,
                writeAllFields: true
            }
        },
        autoSync: false
    });

    var elements = schema.get('elements') || [];

    var nodes = elements.map(function(element) {
        var model = graph.elementStore.add(element)[0];
        model.commit();  //tell EXT this guy is in sync with the server
        return createCell(model);
    });

    var connections = schema.get('connections') || [];
    var wires = connections.map(function(con) {
        var model = graph.connectionStore.add(con)[0];
        model.commit();  //tell EXT this guy is in sync with the server
        return model.graphLink = new joint.shapes.logic.Wire({
            source: { id: graphId(con.source_id),
                      port: 'out' + con.source_output },
            target: { id: graphId(con.target_id),
                      port: 'in' + con.target_input },
            vertices: con.vertices || [],
            con: model
        });
    });

    graph.resetCells(nodes.concat(wires));
    replayStatusCache(graph);
}

function updateElementPosition(cell) {
    var element = cell.attributes.element;
    if(!element) return;
    var pos = cell.position();
    element.set('x', pos.x);
    element.set('y', pos.y);
}

function updateConnection(graph, link) {
    var target = link.attributes.target;
    if(!target.id) return; //not yet connected

    var source = link.attributes.source;
    var vertices = link.attributes.vertices || [];
    if(!link.attributes.con) {
        //new link
        link.attributes.con = graph.connectionStore.add({
            source_id: parseId(source.id),
            source_output: parsePort(source.port),
            target_id: parseId(target.id),
            target_input: parsePort(target.port),
            vertices: vertices
        })[0];
    } else {
        //updated link
        var model = link.attributes.con;
        model.set("source_id", parseId(source.id));
        model.set("source_output", parsePort(source.port));
        model.set("target_id", parseId(target.id));
        model.set("target_input", parsePort(target.port));
        model.set("vertices", vertices);
    }
    link.attributes.con.graphLink = link;
}

function removeConnection(graph, link) {
    if(!graph.connectionStore) return;   //don't remove when switching schema
    var target = link.attributes.target;
    if(!target || !target.id || !link.attributes.con) return; //not yet connected
    link.attributes.con.drop();
}

function commitSchemaChanges(graph) {
    if(graph.elementStore) {
        graph.elementStore.sync();
    }
    if(graph.connectionStore) {
        graph.connectionStore.sync();
    }
}

function createSchemaToolbarHandler(graph, type) {
    return function() {
        var model = graph.elementStore.add({
            type: type,
            x: 50, y: 50
        })[0];
        graph.elementStore.sync({
            callback: function() {
                graph.addCell(createCell(model));
            }
        });
    };
}

function createSchemaToolbar(graph) {
    var gates = [];
    for(var type in TYPE2SHAPE) {
        gates.push({
            text: type,
            handler: createSchemaToolbarHandler(graph, type)
        });
    }
    return Ext.create('Ext.toolbar.Toolbar', {
        cls: 'toolbar',
        items: gates
    });
}

function saveSchema(schema, elementStore, connectionStore) {
    if(elementStore) {
        schema.data.elements = elementStore.data.items.map(function(x) {return x.data;});
    }
    if(connectionStore) {
        schema.data.connections = connectionStore.data.items.map(function(x) {return x.data;});
    }
}

function updateStatusCache(graph, message) {
    graph.statusCache[message.id] = message;
}

function replayStatusCache(graph) {
    if(!graph.paper) return;
    for(var id in graph.statusCache) {
        var message = graph.statusCache[id];
        handleNotif(graph, graph.paper, message);
    }
}

function handleNotif(graph, paper, message) {
    updateStatusCache(graph, message);

    var cell;
    if(message.type == 'relay' || message.type == 'switch') {
        cell = graph.getCell(graphId(message.id));
    } else if(message.type == 'connection' && graph.connectionStore) {
        var element = graph.connectionStore.getById(message.id);
        if(element) {
            cell = graph.connectionStore.getById(message.id).graphLink;
        }
    }

    if(cell) {
        var view = V(paper.findViewByModel(cell).el);
        view.toggleClass('on', message.value);
        view.toggleClass('off', !message.value);
    }
}

function handleClick(graph, cell) {
    var element = cell.model.attributes.element;
    if(!element) return;
    var type = element.get("type");
    if(type == 'switch') {
        Ext.Ajax.request({
            url: '/controls/toggle/' + element.get('id'),
            method : "PUT",
            headers: {'Content-Type': 'application/json'},
            jsonData: true
        });
        //TODO: send message to server
        console.log('toggle '+element.get('id'));
    }
}

function createSchema(name, grid) {
    var graph = new joint.dia.Graph;
    graph.statusCache = {};

    var toolbar = createSchemaToolbar(graph);
    toolbar.disable();

    var prevSchema = undefined;
    grid.getSelectionModel().on('selectionchange',
        function(selModel, selections)  {
            if(prevSchema) saveSchema(prevSchema, graph.elementStore, graph.connectionStore);
            delete graph.elementStore;
            delete graph.connectionStore;
            graph.clear();

            if(selections.length > 0 && !selections[0].phantom) {
                prevSchema = selections[0];
                toolbar.enable();
                loadGraph(graph, selections[0]);
            } else {
                toolbar.disable();
            }
        }
    );

    var batches = 0;
    graph.on('change:position', updateElementPosition);
    graph.on('change:source', function(link) {updateConnection(graph, link);});
    graph.on('change:target', function(link) {updateConnection(graph, link);});
    graph.on('change:vertices', function(link) {updateConnection(graph, link);});
    graph.on('remove', function(link) {removeConnection(graph, link);});
    graph.on('batch:start', function() {batches++;});
    graph.on('batch:stop', function() {
        if(--batches == 0) commitSchemaChanges(graph);
    });

    return {
        region: 'center',
        html : '<div id="paper" class="paper" style="width: 100%; height: 100%"/>',
        dockedItems: [toolbar],
        listeners: {
            afterlayout: function() {
                var paper = new joint.dia.Paper({
                    el: $('#paper'),
                    width: '100%',
                    height: '100%',
                    model: graph,
                    gridSize: 5,
                    snapLinks: true,
                    defaultLink: new joint.shapes.logic.Wire,
                    validateConnection: function(vs, ms, vt, mt, e, vl) {
                        if (e === 'target') {
                            // target requires an input port to connect
                            if (!mt || !mt.getAttribute('class') || mt.getAttribute('class').indexOf('input') < 0) return false;

                            // check whether the port is being already used
                            var portUsed = _.find(this.model.getLinks(), function(link) {
                                return (link.id !== vl.model.id &&
                                        link.get('target').id === vt.model.id &&
                                        link.get('target').port === mt.getAttribute('port'));
                            });

                            return !portUsed;
                        } else { // e === 'source'

                            // source requires an output port to connect
                            return ms && ms.getAttribute('class') && ms.getAttribute('class').indexOf('output') >= 0;
                        }
                    }
                });
                graph.paper = paper;
                var notifListener = new MyWebSocket({
                    url: 'ws://' + window.location.host + '/notifs'
                });
                notifListener.on('message', function(message) {
                    handleNotif(graph, paper, Ext.decode(message));
                });
                paper.on('cell:pointerclick', function(cell) {
                    handleClick(graph, cell);
                });
            }
        }
    };
}
