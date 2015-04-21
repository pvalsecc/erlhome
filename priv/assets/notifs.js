Ext.require(['Ext.util.Observable']);

Ext.define('NotifsListener', {
    mixins: {
        observable: 'Ext.util.Observable'
    },

    constructor: function (config) {
        var self = this;
        this.mixins.observable.constructor.call(this, config);
        this.ws = new WebSocket('ws://' + window.location.host + '/notifs');
        this.ws.onmessage = function(event) { self.onMessage(event.data); };
    },

    onMessage: function(event) {
        console.log(event);
    }
});

var notifs = new NotifsListener({});
