package ch.thus.erlhome;

import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.util.Log;

import com.loopj.android.http.AsyncHttpClient;
import com.loopj.android.http.JsonHttpResponseHandler;

import org.apache.http.Header;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Controller extends JsonHttpResponseHandler {
    private static final String TAG = "Controller";
    private WebSocketClient ws;
    private final AsyncHttpClient client = new AsyncHttpClient();
    private URI baseUrl;
    private final Map<Integer, Schema> schemas = new HashMap<>();
    private FloorsPagerAdapter pagerAdapter;


    public Controller(FloorsPagerAdapter pagerAdapter, String serverAddress) {
        this.pagerAdapter = pagerAdapter;
        client.setLoggingEnabled(true);
        connect(serverAddress);
    }

    private void connect(String serverAddress) {
        final URI wsUri;
        try {
            baseUrl = new URI(serverAddress);
            wsUri = new URI(baseUrl.resolve("/notifs").toString().replaceFirst("https?://", "ws://"));
        } catch (URISyntaxException e) {
            Log.e(TAG, "Bad", e);
            return;
        }
        Log.i(TAG, "Connecting to " + baseUrl.toString());
        client.get(getUrl("schemas"), this);
        ws = new WebSocketClient(wsUri) {
            @Override
            public void onOpen(ServerHandshake handshakedata) {
                Log.i(TAG, "WebSocket open: " + wsUri.toString());
                send("subscribe [status, switch, all]");
                send("subscribe [status, desc, all]");
            }

            @Override
            public void onMessage(String message) {
                Log.i(TAG, message);
                try {
                    JSONObject json = new JSONObject(message);
                    dispatch(json);
                } catch (JSONException e) {
                    Log.e(TAG, "Error parsing  WS", e);

                }
            }

            @Override
            public void onClose(int code, String reason, boolean remote) {
                Log.w(TAG, "Close");
            }

            @Override
            public void onError(Exception ex) {
                Log.w(TAG, "Error", ex);
            }
        };
    }

    private synchronized void dispatch(JSONObject json) throws JSONException {
        int schemaId = json.getJSONArray("path").getInt(2);
        Schema schema = schemas.get(schemaId);
        if (schema != null) {
            schema.onMessage(json);
        }
    }

    private String getUrl(String url) {
        return baseUrl.resolve(url).toString();
    }

    public synchronized void onSuccess(int statusCode, Header[] headers, JSONArray response) {
        Log.i(TAG, "Got schemas: " + response.toString());
        try {
            schemas.clear();
            for (int i = 0; i < response.length(); i++) {
                JSONObject schema = response.getJSONObject(i);
                schemas.put(schema.getInt("id"), new Schema(baseUrl, schema, client));
            }
            final List<Schema> sortedSchemas = new ArrayList<Schema>(schemas.values());
            Collections.sort(sortedSchemas);
            pagerAdapter.schemaChanges(sortedSchemas);
        } catch (JSONException e) {
            Log.e(TAG, "Error handling schemas", e);
        }
        ws.connect();
    }
}
