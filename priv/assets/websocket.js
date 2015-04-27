Ext.require(['Ext.util.Observable']);

Ext.define('MyWebSocket', {
    url: '',

    mixins: {
        observable: 'Ext.util.Observable'
    },

    constructor: function (config) {
        this.mixins.observable.constructor.call(this, config);
        var self = this;
        this.open();
        this.scheduleRetry();
    },

    onOpen: function(data) {
        this.stopRetry();
    },

    onMessage: function(data) {
        this.fireEvent("message", data);
    },

    onClose: function(data) {
        this.scheduleRetry();
    },

    scheduleRetry: function(data) {
        if(this.openTask) return;
        var self = this;
        this.openTask = Ext.util.TaskManager.start({
            run: function() {
                self.open();
            },
            interval: 5000
        });
    },

    stopRetry: function() {
        if(this.openTask) {
            Ext.util.TaskManager.stop(this.openTask);
            delete this.openTask;
        }
    },

    open: function() {
        this.close();
        this.ws = new WebSocket('ws://' + window.location.host + '/notifs');

        var self = this;
        this.ws.onopen = function(event) { self.onOpen(event); };
        this.ws.onmessage = function(event) { self.onMessage(event.data); };
        this.ws.onclose = function(event) { self.onClose(event); };
    },

    close: function() {
        if(this.ws) {
            this.ws.close();
        }
    }
});
