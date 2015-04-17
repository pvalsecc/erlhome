Ext.require(['Ext.data.*', 'Ext.grid.*']);

Ext.application({
    name   : 'MyApp',

    launch : function() {
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
                                items: [createSchemasGrid()]
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

        createSchema('#paper');
    }
});

function createSchema(name) {
    var graph = new joint.dia.Graph;

    var paper = new joint.dia.Paper({
        el: $('#paper'),
        width: '100%',
        height: '100%',
        model: graph,
        gridSize: 1
    });

    var gates = {
        repeater: new joint.shapes.logic.Repeater({ position: { x: 350, y: 50 }}),
        or: new joint.shapes.logic.Or({ position: { x: 550, y: 50 }}),
        and: new joint.shapes.logic.And({ position: { x: 550, y: 150 }}),
        not: new joint.shapes.logic.Not({ position: { x: 120, y: 200 }}),
        nand: new joint.shapes.logic.Nand({ position: { x: 550, y: 250 }}),
        nor: new joint.shapes.logic.Nor({ position: { x: 250, y: 130 }}),
        xor: new joint.shapes.logic.Xor({ position: { x: 550, y: 200 }}),
        xnor: new joint.shapes.logic.Xnor({ position: { x: 550, y: 100 }}),
        input: new joint.shapes.logic.Input({ position: { x: 10, y: 100 }}),
        output: new joint.shapes.logic.Output({ position: { x: 400, y: 300 }})
    }

    var wires = [
        { source: { id: gates.input.id, port: 'out' }, target: { id: gates.not.id, port: 'in' }},
        { source: { id: gates.not.id, port: 'out' }, target: { id: gates.nor.id, port: 'in1' }},
        { source: { id: gates.nor.id, port: 'out' }, target: { id: gates.repeater.id, port: 'in' }},
        { source: { id: gates.nor.id, port: 'out' }, target: { id: gates.output.id, port: 'in' }},
        { source: { id: gates.repeater.id, port: 'out' }, target: { id: gates.nor.id, port: 'in2'},
          vertices: [{ x: 300, y: 220 }]
        }
    ];

    graph.addCells(_.toArray(gates));
    _.each(wires, function(attributes) { graph.addCell(new joint.shapes.logic.Wire(attributes)) });
}

Ext.define('Schema', {
    extend: 'Ext.data.Model',
    fields: [
        'id',
        'name',
        'href'
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
                writeRecordId: false
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
