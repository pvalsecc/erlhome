Ext.require(['Ext.data.*', 'Ext.grid.*']);

Ext.application({
    name   : 'MyApp',

    launch : function() {
       var schemaGrid = createSchemasGrid();

       Ext.create('Ext.container.Viewport', {
            renderTo     : Ext.getBody(),
            width        : '100%',
            height       : '100%',
            layout: 'fit',
            items: [{
                xtype : 'tabpanel',
                bodyPadding  : 5,
                title        : 'Erlhome',
                items        : [
                    {
                        title : 'Schemas',
                        layout: 'border',
                        items : [
                            {
                                region:'west',
                                split: true,
                                width: 200,
                                height: '100%',
                                items: [schemaGrid]
                            },
                            {
                                region: 'center',
                                html : '<div id="paper" class="paper" style="width: 100%; height: 100%"/>'
                            }
                        ]
                    }
                ]
            }]
        });

        createSchema('#paper', schemaGrid);
    }
});

var TYPE2SHAPE = {
    or: joint.shapes.logic.Or,
    and: joint.shapes.logic.And,
    xor: joint.shapes.logic.Xor
};

function graphId(id) {
    return "schema-element-" + id;
}

function loadGraph(graph, schema) {
    graph.clear();

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

    var nodes = schema.get('elements').map(function(element) {
        var model = graph.elementStore.add(element)[0];
        model.commit();  //tell EXT this guy is in sync with the server
        return new TYPE2SHAPE[element.type]({
            id: graphId(element.id),
            position: {x: element.x, y: element.y},
            element: model
        });
    });

    var wires = schema.get('connections').map(function(con) {
        return new joint.shapes.logic.Wire({
            source: { id: graphId(con.source_id),
                      port: 'out' + con.source_output },
            target: { id: graphId(con.target_id),
                      port: 'in' + con.target_input }
        });
    });

    graph.resetCells(nodes.concat(wires));
}

function updateElementPosition(cell) {
    var pos = cell.position();
    var element = cell.attributes.element;
    element.set('x', pos.x);
    element.set('y', pos.y);
    console.log("Moved");
}

function commitSchemaChanges(graph) {
    console.log("Commit");
    if(graph.elementStore) {
        graph.elementStore.sync();
    }
}

function createSchema(name, grid) {
    var graph = new joint.dia.Graph;

    var paper = new joint.dia.Paper({
        el: $('#paper'),
        width: '100%',
        height: '100%',
        model: graph,
        gridSize: 1
    });

    grid.getSelectionModel().on('selectionchange',
        function(selModel, selections)  {
            if(selections.length > 0) {
                loadGraph(graph, selections[0])
            } else {
                graph.clear();
                delete graph.elementStore
            }
        }
    );

    graph.on('change:position', updateElementPosition);
    graph.on('batch:stop', function() {commitSchemaChanges(graph);});
}

Ext.define('Element', {
    extend: 'Ext.data.Model',
    fields: [
        'id',
        'type',
        {name: 'x', type: 'int'},
        {name: 'y', type: 'int'}
    ]
});

Ext.define('Connection', {
    extend: 'Ext.data.Model',
    fields: [
        'id',
        'source_id',
        'source_output',
        'target_id',
        'target_input'
    ]
});

Ext.define('Schema', {
    extend: 'Ext.data.Model',
    fields: [
        'id',
        'name',
        'href',
        'connections',
        'elements'
        ],
    validators: {
        name: 'presence'
    }
});

function createSchemasStore() {
    var store = new Ext.data.Store({
        storeId: 'Schemas',

        proxy: {
            type: 'rest',
            url: 'schemas',
            reader: {
                type: 'json'
            },
            writer: {
                type: 'json',
                writeRecordId: false,
                writeAllFields: true
            }
        },

        model: 'Schema',
        autoLoad: true,
        autoSync: true
    });
    return store;
}

function createSchemasGrid() {
    var schemasStore = createSchemasStore();

    var rowEditing = Ext.create('Ext.grid.plugin.RowEditing', {
        listeners: {
            cancelEdit: function(rowEditing, context) {
                // Canceling editing of a locally added, unsaved record: remove it
                if (context.record.phantom) {
                    store.remove(context.record);
                }
            }
        }
    });

    var grid = Ext.create('Ext.grid.Panel', {
        width: '100%',
        height: '100%',
        plugins: [rowEditing],
        store: schemasStore,
        columns: [
            {text: 'Schema Name', dataIndex: 'name', width: '100%', editor: 'textfield'}
        ],
        plugins: {
            ptype: 'rowediting',
            //clicksToEdit: 1
        },
        dockedItems: [{
            xtype: 'toolbar',
            items: [{
                text: 'Add',
                iconCls: 'icon-add',
                handler: function(){
                    // empty record
                    schemasStore.insert(0, new Schema());
                    rowEditing.startEdit(0, 0);
                }
            }, '-', {
                itemId: 'delete',
                text: 'Delete',
                iconCls: 'icon-delete',
                disabled: true,
                handler: function(){
                    var selection = grid.getView().getSelectionModel().getSelection()[0];
                    if (selection) {
                        schemasStore.remove(selection);
                    }
                }
            }]
        }]
    });

    grid.getSelectionModel().on('selectionchange', function(selModel, selections){
        grid.down('#delete').setDisabled(selections.length === 0);
    });

    return grid;
}
