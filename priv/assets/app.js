Ext.require(['Ext.data.*', 'Ext.grid.*']);

Ext.application({
    name   : 'MyApp',

    launch : function() {
       var schemaGrid = createSchemasGrid();
       var devicesGrid = createDevicesGrid();

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
                                region: 'west',
                                split: true,
                                width: 200,
                                height: '100%',
                                items: [schemaGrid]
                            },
                            createSchema('#paper', schemaGrid)
                        ]
                    },
                    {
                        title: 'Devices',
                        layout: 'border',
                        items: [
                            {
                                region: 'west',
                                split: true,
                                width: 200,
                                height: '100%',
                                items: [devicesGrid]
                            }
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
        {name: 'y', type: 'int'},
        'config'
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
        'href'
    ],
    validators: {
        name: 'presence'
    }
});

Ext.define('Zwave', {
    extend: 'Ext.data.Model',
    fields: ['id', 'name', 'desc'],
});

function createSubStore(model, urlPostfix, schemaId) {
    return Ext.create('Ext.data.Store', {
        model: model,
        proxy: {
            type: 'rest',
            url: "/schemas/" + schemaId + '/' + urlPostfix,
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
}

function createSchemasStore() {
    return new Ext.data.Store({
        storeId: 'Schemas',

        proxy: {
            type: 'rest',
            url: '/schemas',
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
}

function createZwaveStore(type) {
    return new Ext.data.Store({
        storeId: 'Zwave/' + type,

        proxy: {
            type: 'rest',
            url: '/zwave/' + type,
            reader: {
                type: 'json'
            },
            writer: {
                type: 'json',
                writeRecordId: false,
                writeAllFields: true
            }
        },

        model: 'Zwave',
        autoLoad: true,
        autoSync: true
    });
}

function createRowEditing(store) {
    return Ext.create('Ext.grid.plugin.RowEditing', {
        listeners: {
            cancelEdit: function(editor, context) {
                // Canceling editing of a locally added, unsaved record: remove it
                if (context.record.phantom) {
                    store.remove(context.record);
                }
            }
        }
    });
}

function createSchemasGrid() {
    var schemasStore = createSchemasStore();

    var grid = Ext.create('Ext.grid.Panel', {
        width: '100%',
        height: '100%',
        plugins: [createRowEditing(schemasStore)],
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


function createDevicesGrid() {
    var devicesStore = createZwaveStore('switch_binary');

    return Ext.create('Ext.grid.Panel', {
        width: '100%',
        height: '100%',
        plugins: [createRowEditing(devicesStore)],
        store: devicesStore,
        columns: [
            {text: 'Name', dataIndex: 'name', width: '30%', editor:
            'textfield'},
            {text: 'Description', dataIndex: 'desc', width: '70%', editor:
            'textfield'},
        ],
        dockedItems: [{
            xtype: 'toolbar',
            items: [{
                text: 'Refresh',
                iconCls: 'icon-refresh',
                handler: function() {
                    devicesStore.reload();
                }
            }]
        }]
    });
}
