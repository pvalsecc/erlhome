package ch.thus.erlhome;

import android.app.Fragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.Switch;

import java.util.Collection;

/**
 * A placeholder fragment containing a simple view.
 */
public class FloorFragment extends Fragment {
    private Schema schema;

    public static FloorFragment newInstance(Schema schema) {
        FloorFragment fragment = new FloorFragment();
        fragment.setSchema(schema);
        return fragment;
    }

    public FloorFragment() {
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View rootView = inflater.inflate(R.layout.fragment_floors, container, false);
        update(rootView);
        return rootView;
    }

    public void update() {
        getActivity().runOnUiThread(new Runnable() {
            @Override
            public void run() {
                update(getView());
            }
        });
    }

    private void update(View view) {
        LinearLayout layout = (LinearLayout) view.findViewById(R.id.switches_layout);
        layout.removeAllViews();
        if(schema == null) return;
        Collection<Element> elements = schema.getSwitches();
        for(Element elem: elements) {
            addSwitch(layout, elem);
        }
    }

    private void addSwitch(LinearLayout layout, Element elem) {
        ElementView button = new ElementView(getActivity());
        button.setElement(elem);
        layout.addView(button);
    }

    public void setSchema(Schema schema) {
        this.schema = schema;
        schema.setView(this);
    }
}
