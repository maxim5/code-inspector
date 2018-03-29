/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package util;

import java.util.*;
import dmoz.util.Category;

/**
 *
 * @author hoshun
 */
public class SpecifiedTopics {

    public SpecifiedTopics() {
        topics = new TreeSet<Category>();
        
        topics.add(new Category("Arts", "Animation"));
        topics.add(new Category("Arts", "Architecture"));
        topics.add(new Category("Arts", "Art"));
        topics.add(new Category("Arts", "Comics"));
        topics.add(new Category("Arts", "Crafts"));
        topics.add(new Category("Arts", "Design"));
        topics.add(new Category("Arts", "Education"));
        topics.add(new Category("Arts", "Illustration"));
        topics.add(new Category("Arts", "Literature"));
        topics.add(new Category("Arts", "Movies"));
        topics.add(new Category("Arts", "Music"));
        topics.add(new Category("Arts", "Online"));
        topics.add(new Category("Arts", "People"));
        topics.add(new Category("Arts", "Performing"));
        topics.add(new Category("Arts", "Photography"));
        topics.add(new Category("Arts", "Radio"));
        topics.add(new Category("Arts", "Television"));
        topics.add(new Category("Arts", "Visual"));
        topics.add(new Category("Arts", "Writers"));
        
        topics.add(new Category("Society", "Crime"));
        topics.add(new Category("Society", "Death"));
        topics.add(new Category("Society", "Ethnicity"));
        topics.add(new Category("Society", "Gay"));
        topics.add(new Category("Society", "Genealogy"));
        topics.add(new Category("Society", "Government"));
        topics.add(new Category("Society", "History"));
        topics.add(new Category("Society", "Holidays"));
        topics.add(new Category("Society", "Issues"));
        topics.add(new Category("Society", "Law"));
        topics.add(new Category("Society", "Military"));
        topics.add(new Category("Society", "Organizations"));
        topics.add(new Category("Society", "Paranormal"));
        topics.add(new Category("Society", "People"));
        topics.add(new Category("Society", "Philanthropy"));
        topics.add(new Category("Society", "Philosophy"));
        topics.add(new Category("Society", "Politics"));
        topics.add(new Category("Society", "Relationships"));
        topics.add(new Category("Society", "Religion"));
        topics.add(new Category("Society", "Subcultures"));
        
        topics.add(new Category("Science", "Agriculture"));
        topics.add(new Category("Science", "Astronomy"));
        topics.add(new Category("Science", "Biology"));
        topics.add(new Category("Science", "Chemistry"));
        topics.add(new Category("Science", "Earth"));
        topics.add(new Category("Science", "Environment"));
        topics.add(new Category("Science", "Instruments"));
        topics.add(new Category("Science", "Math"));
        topics.add(new Category("Science", "Physics"));
        topics.add(new Category("Science", "Social"));
        topics.add(new Category("Science", "Technology"));
        
        topics.add(new Category("Computers", "Artificial"));
        topics.add(new Category("Computers", "Computer"));
        topics.add(new Category("Computers", "Consultants"));
        topics.add(new Category("Computers", "Data"));
        topics.add(new Category("Computers", "Graphics"));
        topics.add(new Category("Computers", "Hardware"));
        topics.add(new Category("Computers", "Internet"));
        topics.add(new Category("Computers", "Multimedia"));
        topics.add(new Category("Computers", "Programming"));
        topics.add(new Category("Computers", "Security"));
        topics.add(new Category("Computers", "Software"));
        topics.add(new Category("Computers", "Systems"));

    }
    
    public boolean containsCategory(Category obj){
        return topics.contains(obj);
    }
        
    private Set<Category> topics;
}
