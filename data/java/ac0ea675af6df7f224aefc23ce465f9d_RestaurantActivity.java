package finappsparty.mobileat.client;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentTransaction;
import android.widget.TextView;
import android.widget.Toast;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockActivity;
import com.actionbarsherlock.app.ActionBar.Tab;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.widget.ShareActionProvider;

import finappsparty.mobileat.client.fragments.RestaurantDetailFragment;
import finappsparty.mobileat.client.fragments.RestaurantOpinionsFragment;

public class RestaurantActivity extends SherlockFragmentActivity {
  
  @Override
  public void onCreate(Bundle savedInstanceState){
      super.onCreate(savedInstanceState);
      setContentView(R.layout.restaurant_activity);
      
      ActionBar ab = getSupportActionBar();
      ab.setNavigationMode(ActionBar.NAVIGATION_MODE_TABS);
      ab.setDisplayOptions(ActionBar.DISPLAY_SHOW_TITLE|ActionBar.DISPLAY_SHOW_HOME|ActionBar.DISPLAY_HOME_AS_UP);
      ab.setTitle("Restaurant");
      
      ActionBar.Tab screenOneTab = ab.newTab().setText("Details");
      ActionBar.Tab screenTwoTab = ab.newTab().setText("Opinions");
      
      Fragment screenOneFragment = new RestaurantDetailFragment();
      Fragment screenTwoFragment = new RestaurantOpinionsFragment();
      
      //set the Tab listener. Now we can listen for clicks
      screenOneTab.setTabListener(new MyTabsListener(screenOneFragment));
      screenTwoTab.setTabListener(new MyTabsListener(screenTwoFragment));
      
      //add the two tabs to the actionbar
      ab.addTab(screenOneTab);
      ab.addTab(screenTwoTab);
  }
  
  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
      MenuInflater inflater = getSupportMenuInflater();
      inflater.inflate(R.menu.restaurant_menu, menu);
      return true;
  }

  /**
   * Creates a sharing {@link Intent}.
   */
  private Intent createShareIntent() {
      Intent shareIntent = new Intent(android.content.Intent.ACTION_SEND);
      shareIntent.setType("text/plain");
      String shareBody = "Text for share with restaurant name";
      shareIntent.putExtra(android.content.Intent.EXTRA_SUBJECT, "MobilEat");
      shareIntent.putExtra(android.content.Intent.EXTRA_TEXT, shareBody);
      return shareIntent;
  }

  @Override
  public boolean onOptionsItemSelected(MenuItem item) {
    switch (item.getItemId()) {
      case android.R.id.home:
        this.finish();
        return true;
      case R.id.menu_map:
        Intent intent = new Intent(this, MapsActivity.class);
        Bundle b = new Bundle();
        b.putBoolean("clicable_btn", false); 
        intent.putExtras(b);
        startActivity(intent);
        return true;
      case R.id.menu_share:
      //create the send intent  
        Intent shareIntent =   
         new Intent(android.content.Intent.ACTION_SEND);  
          
        //set the type  
        shareIntent.setType("text/plain");  
          
        //add a subject  
        shareIntent.putExtra(android.content.Intent.EXTRA_SUBJECT,   
         "Restaurant Invictus - MobilEat");  
          
        //build the body of the message to be shared  
        String shareMessage = "I discovered this amazing restaurant from MobilEat app!!";  
          
        //add the message  
        shareIntent.putExtra(android.content.Intent.EXTRA_TEXT,   
         shareMessage);  
          
        //start the chooser for sharing  
        startActivity(Intent.createChooser(shareIntent,   "shareChoserTitle"));  
        return true;
    }
    return false;
  }
  
  class MyTabsListener implements ActionBar.TabListener {
    public Fragment fragment;
     
    public MyTabsListener(Fragment fragment) {
      this.fragment = fragment;
    }
     
    @Override
    public void onTabReselected(Tab tab, FragmentTransaction ft) {
    }
     
    @Override
    public void onTabSelected(Tab tab, FragmentTransaction ft) {
      ft.replace(R.id.fragment_container, fragment);
    }
     
    @Override
    public void onTabUnselected(Tab tab, FragmentTransaction ft) {
      ft.remove(fragment);
    }
     
  }
}
