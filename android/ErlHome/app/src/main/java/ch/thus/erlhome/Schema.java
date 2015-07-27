package ch.thus.erlhome;

import android.util.Log;

import com.loopj.android.http.AsyncHttpClient;
import com.loopj.android.http.JsonHttpResponseHandler;

import org.apache.http.Header;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Schema extends JsonHttpResponseHandler implements Comparable<Schema> {
    private static final String TAG = "Schema";
    private final String name;
    private final Map<Integer, Element> switches = new HashMap<Integer, Element>();
    private final URI baseUrl;
    private final AsyncHttpClient client;
    private Map<Integer, List<JSONObject>> messageQueue = new HashMap<Integer, List<JSONObject>>();
    FloorFragment view;

    public Schema(URI baseUrl, JSONObject schema, AsyncHttpClient client) throws JSONException {
        this.client = client;
        this.baseUrl = baseUrl.resolve(schema.getString("href") + "/elements");
        Log.i(TAG, this.baseUrl.toString());
        name = schema.getString("name");
        client.get(this.baseUrl.toString(), this);
    }

    public String getName() {
        return name;
    }

    public Collection<Element> getSwitches() {
        return switches.values();
    }

    public synchronized void onMessage(JSONObject json) throws JSONException {
        int id = json.getJSONArray("path").getInt(3);
        Element element = switches.get(id);
        if (element != null) {
            element.onMessage(json);
        } else {
            queueMessage(id, json);
        }
    }

    private void queueMessage(int id, JSONObject json) {
        if (messageQueue == null) return;
        List<JSONObject> queue = messageQueue.get(id);
        if (queue == null) {
            queue = new ArrayList<JSONObject>(2);
            messageQueue.put(id, queue);
        }
        queue.add(json);
    }

    @Override
    public synchronized void onSuccess(int statusCode, Header[] headers, JSONArray response) {
        Log.i(TAG, response.toString());
        switches.clear();
        for (int i = 0; i < response.length(); i++) {
            try {
                JSONObject element = response.getJSONObject(i);
                String type = element.getString("type");
                if (type.equals("module") || type.equals("switch")) {
                    Integer id = element.getInt("id");
                    Element elem = new Element(baseUrl, element, client);
                    switches.put(id, elem);

                    List<JSONObject> queue = messageQueue.get(id);
                    if (queue != null) {
                        for (JSONObject message : queue) {
                            elem.onMessage(message);
                        }
                    }
                }
            } catch (JSONException e) {
                e.printStackTrace();
            }
        }
        messageQueue = null;
        if (view != null) {
            view.update();
        }
    }

    @Override
    public int compareTo(Schema another) {
        if (another != null)
            return name.compareTo(another.name);
        else
            return 1;
    }

    public void setView(FloorFragment view) {
        this.view = view;
    }
}
