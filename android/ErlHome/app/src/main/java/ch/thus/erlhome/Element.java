package ch.thus.erlhome;

import android.util.Log;

import com.loopj.android.http.AsyncHttpClient;
import com.loopj.android.http.AsyncHttpResponseHandler;
import com.loopj.android.http.RequestParams;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.entity.StringEntity;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.UnsupportedEncodingException;
import java.net.URI;

public class Element {
    private final URI baseUrl;
    private final String TAG;
    private final AsyncHttpClient client;
    private final int id;
    private boolean status = false;
    private String desc;
    private ElementView view;

    public Element(URI parentUrl, JSONObject element, AsyncHttpClient client) throws JSONException {
        this.client = client;
        baseUrl = parentUrl.resolve(element.getString("href"));
        desc = element.getString("id");
        id = element.getInt("id");
        TAG = "Element" + desc;
    }

    public void onMessage(JSONObject json) throws JSONException {
        String what = json.getJSONArray("path").getString(1);
        if (what.equals("switch")) {
            status = json.getBoolean("value");
            Log.v(TAG, "New status: " + Boolean.toString(status));
        } else if (what.equals("desc")) {
            desc = json.getString("value");
            Log.v(TAG, "New desc: " + desc);
        }
        if(view != null) view.update();
    }

    public boolean isStatus() {
        return status;
    }

    public String getDesc() {
        return desc;
    }

    public void setView(ElementView view) {
        this.view = view;
    }

    public void toggle() {
        URI url = baseUrl.resolve("/controls/toggle/" + String.valueOf(id));
        try {
            client.put(null, url.toString(), new StringEntity("true"), "application/json", new AsyncHttpResponseHandler() {
                @Override
                public void onSuccess(int statusCode, Header[] headers, byte[] responseBody) {
                    Log.v(TAG, "Toggle success");
                }

                @Override
                public void onFailure(int statusCode, Header[] headers, byte[] responseBody, Throwable error) {
                    Log.w(TAG, "Toggle failure", error);
                }
            });
        } catch (UnsupportedEncodingException e) {
            Log.e(TAG, "Encoding", e);
        }
    }
}
