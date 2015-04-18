var TYPE2SHAPE = {
    or: joint.shapes.logic.Or,
    and: joint.shapes.logic.And,
    xor: joint.shapes.logic.Xor
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
    graph.clear();

    if(!schema) {
        return;
    }

    //TODO: when creating a new schema, it's ID is unknown yet when we arrive here
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
        return new joint.shapes.logic.Wire({
            source: { id: graphId(con.source_id),
                      port: 'out' + con.source_output },
            target: { id: graphId(con.target_id),
                      port: 'in' + con.target_input },
            vertices: con.vertices || [],
            con: model
        });
    });

    graph.resetCells(nodes.concat(wires));
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
}

function removeConnection(link) {
    var target = link.attributes.target;
    if(!target.id || !link.attributes.con) return; //not yet connected
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

function createSchema(name, grid) {
    var graph = new joint.dia.Graph;

    var toolbar = createSchemaToolbar(graph);
    toolbar.disable();

    grid.getSelectionModel().on('selectionchange',
        function(selModel, selections)  {
            if(selections.length > 0) {
                toolbar.enable();
                loadGraph(graph, selections[0])
            } else {
                toolbar.disable();
                graph.clear();
                delete graph.elementStore
                delete graph.connectionStore
            }
        }
    );

    var batches = 0;
    graph.on('change:position', updateElementPosition);
    graph.on('change:source', function(link) {updateConnection(graph, link);});
    graph.on('change:target', function(link) {updateConnection(graph, link);});
    graph.on('change:vertices', function(link) {updateConnection(graph, link);});
    graph.on('remove', removeConnection);
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
            }
        }
    };
}
