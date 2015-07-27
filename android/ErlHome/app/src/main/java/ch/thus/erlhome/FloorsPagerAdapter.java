package ch.thus.erlhome;

import android.app.Fragment;
import android.app.FragmentManager;
import android.support.v13.app.FragmentPagerAdapter;
import android.support.v13.app.FragmentStatePagerAdapter;
import android.util.Log;

import java.util.Collections;
import java.util.List;

/**
 * A {@link FragmentPagerAdapter} that returns a fragment corresponding to
 * one of the sections/tabs/pages.
 */
public class FloorsPagerAdapter extends FragmentStatePagerAdapter {

    private static final String TAG = "SectionsPagerAdapter";
    private final Controller controller;
    private List<Schema> schemas = Collections.EMPTY_LIST;

    public FloorsPagerAdapter(FragmentManager fm) {
        super(fm);
        this.controller = new Controller(this);
    }

    @Override
    public Fragment getItem(int position) {
        Log.v(TAG, "getItem("+String.valueOf(position)+")");
        // getItem is called to instantiate the fragment for the given page.
        // Return a PlaceholderFragment (defined as a static inner class below).
        return FloorFragment.newInstance(this.schemas.get(position));
    }

    @Override
    public int getCount() {
        return schemas.size();
    }

    public void schemaChanges(List<Schema> schemas) {
        this.schemas = schemas;
        notifyDataSetChanged();
    }
}
