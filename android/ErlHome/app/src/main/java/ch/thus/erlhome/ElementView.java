package ch.thus.erlhome;

import android.content.Context;
import android.widget.CompoundButton;
import android.widget.Switch;

public class ElementView extends Switch {
    private Element element;

    public ElementView(Context context) {
        super(context);
    }


    public void setElement(Element element) {
        this.element = element;
        element.setView(this);
        updateImpl();
    }

    public void update() {
        post(new Runnable() {
            @Override
            public void run() {
                updateImpl();
            }
        });
    }

    private void updateImpl() {
        setText(element.getDesc());
        setChecked(element.isStatus());
    }

    @Override
    public boolean performClick() {
        this.element.toggle();
        return true;
    }
}
