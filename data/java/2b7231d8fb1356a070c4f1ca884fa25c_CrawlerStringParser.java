package edu.upenn.cis.bang.indexer;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class CrawlerStringParser {

	public static String getlinks(String url, String in){
		String str = "";
		String out = "INSERT\n" +url + " ";
		
		String searchterm = "<links>";
		String endsearchterm = "</links>";
		int index = in.indexOf(searchterm);
		int endindex = in.indexOf(endsearchterm);
		
		str = in.substring(index + endsearchterm.length() -1, endindex);
		

		int startindex = 0;
		searchterm = "<link>";
		endsearchterm = "</link>";
		
		while(true){
			
			index = str.indexOf(searchterm,startindex);
			//System.out.println("start index is " + startindex);
			if (index ==-1){
				break;
			}
			
			endindex = str.indexOf(endsearchterm,startindex);
			//System.out.println(index + "," + endindex);
			
			String link = str.substring(index + searchterm.length(), endindex);
	
			out += link + " ";
			
			startindex = endindex + endsearchterm.length();
		}	
		out += "\n.\n";
		
		return out;	
	}
	
	public static Map<String, Float> getwords(String in){
		in = getcontent(in);
		String[] array = in.split(" ");
		float max =0;
		
		Map<String, Float> map = new HashMap<String,Float>();
		
		
		
		for (int i=0; i<array.length; i++ ){
			
			String word = array[i];
			
			word = cutword(word);
			
			
			if (!word.equals("\n") && !word.equals("\r\n") && !word.equals(" ") && word.length()!=0){
			
				if (map.get(word) == null){
					map.put(word,(float)1);
				}
				else{
					map.put(word, map.get(word)+ ((float)1));
				}
				//System.out.println("word: "+ array[i]+ "f:" + map.get(array[i]));
				
				
				if (map.get(word) > max){
					max = map.get(word);
					//System.out.println("max word is :" + array[i]);
					//System.out.println(array[i].length()!=0);
				}
			}
			
		}
		
		Iterator<String> it = map.keySet().iterator();
		float sumsquare = 0;
		while(it.hasNext()){
			String word = it.next();
			//System.out.println("words is:" + word + " f is " + map.get(word));
			float tf =  map.get(word)/max;
			map.put(word, map.get(word)/max);
			sumsquare += Math.pow(tf,2);
			//System.out.println("afterward f is " + map.get(word));		
		}
		
		float normalize_factor = (float) Math.pow(sumsquare, 0.5); 
		
		// normalize document tf
		it = map.keySet().iterator();
		while(it.hasNext()){
			String word = it.next();
			map.put(word, map.get(word)/normalize_factor);
		}
		
		return map;
	}
	
	public static String getcontent(String in){
		String str = "";
		
		String searchterm = "<content>";
		String endsearchterm = "</content>";
		int index = in.indexOf(searchterm);
		int endindex = in.indexOf(endsearchterm);
		
		str = in.substring(index + endsearchterm.length() -1, endindex);
		
		return str;
		
	}
	
	public static String geturl(String in){
		String str = "";
		
		String searchterm = "<url>";
		String endsearchterm = "</url>";
		int index = in.indexOf(searchterm);
		int endindex = in.indexOf(endsearchterm);
		
		str = in.substring(index + endsearchterm.length() -1, endindex);
		
		return str;
	}
	
	
	public static String cutword(String in){
		in = in.toLowerCase();
		return removecomma(in);
		
	}
	
	public static String removecomma(String in){
		while (true){
			int index = in.indexOf(",");
			if(index==-1){
				break;
			}
			
			in =  in.substring(0, index) + in.substring(index+1);
		}
		return in;
		
	}
	
	
	
	public static boolean checkword(String in){
		if (in.length()> 20 || in.length()==1 || in.length()<3){
			return false;
		}
		
		Set<String> s = new HashSet<String>();
		s.add("="); s.add("-");
		s.add("!"); s.add(")");
		s.add("<"); s.add("(");
		s.add(">"); s.add("[");
		s.add("%"); s.add("]");
		s.add("^"); s.add("{");
		s.add("&"); s.add("}");
		s.add("\"");s.add("$");
		s.add("'"); s.add("*");
		s.add("\\");s.add("|");
		s.add(":");
		s.add(";");
		
		Iterator<String> it = s.iterator();
		while(it.hasNext()){
			String next = it.next();
			//System.out.println("next is" + next);
			if (in.contains(next)){
				return false;
			}
		}
		
		return true;
			
	}
	
	
	
	public static void main(String[] args){
		
		/// Test for getting links
		
		String in = "<type>store</type><page><url>www.goggle.com</url><title>page title</title><links><link>www.yahoo.com</link><link>www.gmail.com</link></links><content>Hello Hello Hello how are you doinf people</content></page>";
		
		/*
		String in = "<type>store</type><page><url>www.goggle.com</url><title>page title</title><links><link>www.yahoo.com</link><link>www.gmail.com</link></links> <type>store</type><page><url>www.goggle.com</url><title>page title</title><links> <link>www.yahoo.com</link> <link>www.gmail.com</link></links><content>GizmodoTOP STORIESPlease confirm your birth date:Please enter a valid datePlease enter your full birth yearThis content is restricted.appsShare this post ?&#x2014;    *    *    *    * Share on Tumblr    * submit to reddit    *    * Email this postThe New Essential Apps April 2011: iPhone, Android, iPad and Windows PhoneGizmodo Staff â&#x20AC;&#x201D; The New Essential Apps April 2011: iPhone, Android, iPad and Windows PhoneiPhones. iPads. Android. And Windows Phone 7! We've updated all of our essential apps lists to include a few forgotten favorites, some long awaited arrivals and, as always, even more amazing apps. Be sure to check out all the lists!The New Essential Apps April 2011: iPhone, Android, iPad and Windows PhoneThe Best iPhone AppsPhotosynth, a super awesome panorama app from Microsoft, and Seamless, an app that solves an annoying problem, join a bunch of other apps in our best iPhone apps list. Check out what's new here. [Gizmodo]</content></page>";
		
		
		System.out.println("Contents is: " + getcontent(in));
		System.out.println("words are :" + getwords(in));
		*/
		
		//System.out.println(checkword("f(d"));
		//System.out.println("f(c".contains("("));
		System.out.println(getlinks("www.google.com",in));
	}
	
}
