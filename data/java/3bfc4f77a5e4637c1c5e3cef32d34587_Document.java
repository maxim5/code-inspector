/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package topicmodel;

import java.util.*;
import java.io.*;
import dmoz.util.Category;
import java.util.regex.*;

/**
 * 1. Use specific criteria to select pages from extended cluster to make the
 * document. 2. The document name presents the id and labels
 * clusterid-Cat1.1:Cat1.2-Cat2.1:Cat2.2 .... Document name and the page content
 * are seperated by space. 3.
 *
 * @author hoshun
 */
public class Document {

    public static void main(String[] args) {
        String url = "[Arts:Animatin]xyx";

        String docname = "";
        //Pattern p = Pattern.compile("\\[(.+?):(.+?)(\\].*)");
        Pattern p = Pattern.compile("\\[(.+?):(.+)\\].*");
        Matcher match = p.matcher(url);
        if (match.find()) {
            for (int i = 1; i <= match.groupCount(); i++) {
                System.out.println("index " + i + " : " + match.group(i));
            }
        }
    }

    public Document(String line, Map<Integer, String> urlidToPageContent) {
        _setSelectCategorySet();

        this.urlidToPageContent = urlidToPageContent;
        this.docContentBuilder = new StringBuilder();
        this.categories = new TreeSet<Category>();


        String[] tokens = line.split("\t");

        int clusterid = Integer.parseInt(tokens[0]);
        this.documentID = clusterid;

        int urlid = Integer.parseInt(tokens[2]);
        String url = tokens[3];

        if (urlidToPageContent.containsKey(urlid)) {
            String pageContent = urlidToPageContent.get(urlid);
            docContentBuilder.append(" ");
            docContentBuilder.append(pageContent);
        }
        Category cat = getCategoryFromMarkedURL(url);
        if (cat != null) {
            categories.add(cat);
        }
    }

    public String fillDocument(Scanner in) throws IOException {
        boolean hitNextCluster = false;
        String line = "";
        while (in.hasNextLine() && !hitNextCluster) {
            line = in.nextLine();
            String[] tokens = line.split("\t");
            int oClusterId = Integer.parseInt(tokens[0]);

            if (documentID == oClusterId) {

                int step = Integer.parseInt(tokens[1]);
                //----------- Select criteria!--------------
                if (step == 0) {
                    int urlid = Integer.parseInt(tokens[2]);
                    String url = tokens[3];

                    if (urlidToPageContent.containsKey(urlid)) {
                        String pageContent = urlidToPageContent.get(urlid);

                        docContentBuilder.append(" ");
                        docContentBuilder.append(pageContent);


                    }
                    Category cat = getCategoryFromMarkedURL(url);
                    if (cat != null) {
                        categories.add(cat);
                    }
                }
                //------------------------------------------
            } else {
                hitNextCluster = true;
            }
        }

        if (hitNextCluster) {
            return line;
        } else {
            return null;
        }

    }

    public boolean isFilter() {
        boolean filtered = false;
        int nOfWord = docContentBuilder.toString().split(" ").length - 1;
        int nOfToptopics = topleveltopics();
        filtered = filtered || (nOfWord >= 50000) || (nOfWord == 0)
                || nOfToptopics == 0
                || nOfToptopics > 3
                || !inSelectSet();
        return filtered;
    }

    private int topleveltopics() {
        Set<String> topics = new TreeSet<String>();
        for (Category cat : categories) {
            topics.add(cat.first);
        }
        return topics.size();
    }

    private boolean inSelectSet() {
        boolean in = false;
        for (Category cat : this.categories) {
            if (selectCategories.contains(cat)) {
                in = true;
                break;
            }
        }
        return in;
    }

    public void printDocumentLine(PrintWriter out) {
        out.printf("%d", documentID);
        for (Category cat : categories) {
            out.printf("-%s:%s", cat.first, cat.second);
        }
        // notice docContentBuilder starts with a space
        out.println(docContentBuilder.toString());
    }

    private Category getCategoryFromMarkedURL(String url) {
        Matcher matcher = extractPattern.matcher(url);
        if (matcher.find()) {
            return new Category(matcher.group(1), matcher.group(2));
        } else {
            return null;
        }
    }

    private static void _setSelectCategorySet() {
        Set<Category> cats = new TreeSet();
        cats.add(new Category("Arts", "Architecture"));
        cats.add(new Category("Arts", "Literature"));
        cats.add(new Category("Arts", "Movies"));
        cats.add(new Category("Arts", "Music"));
        cats.add(new Category("Arts", "Performing"));
        cats.add(new Category("Arts", "Television"));
        cats.add(new Category("Computers", "Computer"));
        cats.add(new Category("Computers", "Internet"));
        cats.add(new Category("Computers", "Programming"));
        cats.add(new Category("Computers", "Security"));
        cats.add(new Category("Computers", "Software"));
        cats.add(new Category("Science", "Astronomy"));
        cats.add(new Category("Science", "Biology"));
        cats.add(new Category("Science", "Chemistry"));
        cats.add(new Category("Science", "Earth"));
        cats.add(new Category("Science", "Environment"));
        cats.add(new Category("Science", "Math"));
        cats.add(new Category("Science", "Physics"));
        cats.add(new Category("Science", "Social"));
        cats.add(new Category("Science", "Technology"));
        cats.add(new Category("Society", "Ethnicity"));
        cats.add(new Category("Society", "Government"));
        cats.add(new Category("Society", "History"));
        cats.add(new Category("Society", "Issues"));
        cats.add(new Category("Society", "Law"));
        cats.add(new Category("Society", "People"));
        cats.add(new Category("Society", "Philosophy"));
        cats.add(new Category("Society", "Religion"));

        selectCategories = cats;

    }
    private static Set<Category> selectCategories;
    private static Pattern extractPattern = Pattern.compile("\\[(.+?):(.+?)(\\].*)");
    private Map<Integer, String> urlidToPageContent;
    private Set<Category> categories;
    public StringBuilder docContentBuilder; // begin with space
    public int documentID;
}
