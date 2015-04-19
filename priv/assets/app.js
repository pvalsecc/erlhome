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
                            createSchema('#paper', schemaGrid)
                        ]
                    }
                ]
            }]
        });
    }
});

Ext.define('Element', {
    extend: 'Ext.data.Model',
    fields: [
        {name: 'id', type: 'int'},
        'type',
        {name: 'x', type: 'int'},
        {name: 'y', type: 'int'}
    ]
});

Ext.define('Connection', {
    extend: 'Ext.data.Model',
    fields: [
        {name: 'id', type: 'int'},
        {name: 'source_id', type: 'int'},
        {name: 'source_output', type: 'int'},
        {name: 'target_id', type: 'int'},
        {name: 'target_input', type: 'int'},
        'vertices'
    ]
});

Ext.define('Schema', {
    extend: 'Ext.data.Model',
    fields: [
        {name: 'id', type: 'int'},
        'name',
        'href',
        'connections', //{name: 'connections', defaultValue: []},
        'elements', //{name: 'elements', defaultValue: []}
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
            cancelEdit: function(editor, context) {
                // Canceling editing of a locally added, unsaved record: remove it
                if (context.record.phantom) {
                    schemasStore.remove(context.record);
                }
            },

            edit: function(editor, context) {
                if(context.record.phantom) {
                    new Ext.util.DelayedTask(function() {
                        //deselect to force the user to reselect. Needed to be done later
                        //to work around a bug in EXT.
                        context.grid.getSelectionModel().deselectAll();
                        context.grid.getView().refresh();
                    }).delay(10);
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
        dockedItems: [{
            xtype: 'toolbar',
            items: [{
                text: 'Add',
                iconCls: 'icon-add',
                handler: function() {
                    // empty record
                    schemasStore.insert(0, new Schema());
                    rowEditing.startEdit(0, 0);
                }
            }, '-', {
                itemId: 'delete',
                text: 'Delete',
                iconCls: 'icon-delete',
                disabled: true,
                handler: function() {
                    var selection = grid.getSelectionModel().getSelection()[0];
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
