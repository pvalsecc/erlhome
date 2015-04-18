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
