joint.shapes.logic.Box21 = joint.shapes.logic.IO.extend({
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
            text: {'text-transform': 'none', 'ref-x': 0, 'text-anchor': 'left'}
        }
    }, joint.shapes.logic.IO.prototype.defaults)
});

function createBox21Class(type, text) {
    joint.shapes.logic[type] = joint.shapes.logic.Box21.extend({
        defaults: joint.util.deepSupplement({
            type: 'logic.' + type,
            attrs: {
                text: {text: text}
            }
        }, joint.shapes.logic.Box21.prototype.defaults)
    })
}
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

function createBox11Class(type, text) {
    joint.shapes.logic[type] = joint.shapes.logic.Box11.extend({
        defaults: joint.util.deepSupplement({
            type: 'logic.' + type,
            attrs: {
                text: {text: text}
            }
        }, joint.shapes.logic.Box11.prototype.defaults)
    })
}

createBox21Class('Timer', 'start\nreset');
createBox21Class('DFlipFlop', 'D\nclock');
createBox21Class('ForceOff', 'in\noff');
createBox11Class('UpEdge', 'up edge');
createBox11Class('DownEdge', 'down edge');
createBox11Class('BothEdge', 'both edge');

var TYPE2SHAPE = {
    'switch': joint.shapes.logic.Input,
    relay: joint.shapes.logic.Output,
    or: joint.shapes.logic.Or,
    and: joint.shapes.logic.And,
    xor: joint.shapes.logic.Xor,
    not: joint.shapes.logic.Not,
    timer: joint.shapes.logic.Timer,
    d_flipflop: joint.shapes.logic.DFlipFlop,
    up_edge: joint.shapes.logic.UpEdge,
    down_edge: joint.shapes.logic.DownEdge,
    both_edge: joint.shapes.logic.BothEdge,
    force_off: joint.shapes.logic.ForceOff
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

function createWire(model) {
    return model.graphLink = new joint.shapes.logic.Wire({
        source: { id: graphId(model.get('source_id')),
                  port: 'out' + model.get('source_output') },
        target: { id: graphId(model.get('target_id')),
                  port: 'in' + model.get('target_input') },
        vertices: model.get('vertices') || [],
        con: model
    });
}

function loadGraph(graph, schema) {
    if(!schema) {
        return;
    }

    graph.elementStore = createSubStore('Element', 'elements', schema.get('id'));
    graph.connectionStore =
        createSubStore('Connection', 'connections', schema.get('id'));

    graph.elementStore.load({
        callback: function(records, operation, success) {
            var nodes = success ? records.map(createCell) : [];

            graph.connectionStore.load({
                callback: function(records, operation, success) {
                    var wires = success ? records.map(createWire) : [];
                    graph.resetCells(nodes.concat(wires));
                    replayStatusCache(graph);
                }
            });
        }
    });
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

function removeItem(graph, item) {
    if(!graph.connectionStore) return;   //don't remove when switching schema
    if(item.isLink()) {
        var target = item.attributes.target;
        if(!target || !target.id || !item.attributes.con) return; //not yet connected
        item.attributes.con.drop();
    } else {
        if(!item.attributes.element) return;
        item.attributes.element.drop();
    }
    //graph.commitChanges called by batch:stop
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

function displayTimerForm(graph, element) {
    var form = new Ext.form.Panel({
        title: 'Timer params',
        defaults: {
            padding: 10,
        },
        floating: true,
        draggable: true,
        closable: true,
        defaultType: 'numberfield',
        items: [{
            fieldLabel: 'Delay [ms]',
            name: 'delay',
            minValue: 0
        }],
        buttons: [{
            text: 'Save',
            handler: function() {
                var form = this.up('form');
                var values = form.getForm().getFieldValues();
                element.set('config', values);
                graph.elementStore.sync();
                graph.commitChanges()
                form.close();
            }
        }]
    });
    form.getForm().setValues(element.get("config") || {});
    form.show();
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
    } if(type == 'timer') {
        displayTimerForm(graph, element);
    }
}

function createSchema(name, grid) {
    var graph = new joint.dia.Graph;
    graph.statusCache = {};

    var delay = new Ext.util.DelayedTask(function(){
        if(graph.elementStore) graph.elementStore.sync();
        if(graph.connectionStore) graph.connectionStore.sync();
    });
    graph.commitChanges = function() {
        delay.delay(200);
    }

    var toolbar = createSchemaToolbar(graph);
    toolbar.disable();

    function newGraph(selections) {
            delete graph.elementStore;
            delete graph.connectionStore;
            graph.clear();

            if(selections.length > 0 && !selections[0].phantom) {
                toolbar.enable();
                loadGraph(graph, selections[0]);
            } else {
                toolbar.disable();
            }
    }

    grid.getSelectionModel().on('selectionchange',
        function(selModel, selections)  {
            newGraph(selections);
        }
    );
    grid.getStore().on('write',
        function(store, operation) {
            newGraph(operation.getRecords());
        }
    );

    graph.on('change:position', updateElementPosition);
    graph.on('change:source', function(link) {updateConnection(graph, link);});
    graph.on('change:target', function(link) {updateConnection(graph, link);});
    graph.on('change:vertices', function(link) {updateConnection(graph, link);});
    graph.on('remove', function(link) {removeItem(graph, link);});
    graph.on('batch:stop', function() {
        graph.commitChanges();
    });

    return {
        region: 'center',
        html : '<div id="paper" class="paper" style="width: 100%; height: 100%" oncontextmenu="return false;"/>',
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
                paper.on('cell:pointerup', function(cell, event) {
                    if(event.button >= 2 && !cell.model.isLink()) {
                        cell.model.remove();  //will notify removeItem
                    }
                });
            }
        }
    };
}
