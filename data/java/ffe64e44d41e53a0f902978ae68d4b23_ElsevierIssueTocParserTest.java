package wwmm.pubcrawler.crawlers.elsevier.parsers;

import nu.xom.Builder;
import nu.xom.Document;
import nu.xom.Node;
import org.apache.commons.io.IOUtils;
import org.ccil.cowan.tagsoup.Parser;
import org.junit.AfterClass;
import org.junit.Test;
import wwmm.pubcrawler.crawlers.elsevier.Elsevier;
import wwmm.pubcrawler.model.Issue;
import wwmm.pubcrawler.model.IssueLink;
import wwmm.pubcrawler.model.id.ArticleId;
import wwmm.pubcrawler.model.id.JournalId;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.List;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;

/**
 * @author Sam Adams
 */
public class ElsevierIssueTocParserTest {

    private static final JournalId JOURNAL_09254005 = new JournalId(Elsevier.PUBLISHER_ID, "09254005");
    
    private static Document journal09254005;
    private static Document journal01677322;
    private static Document journal000928674;
    private static Document journal000928674_S2;

    @AfterClass
    public static void afterAllTests() {
        journal09254005 = null;
        journal01677322 = null;
        journal000928674 = null;
        journal000928674_S2 = null;
    }

    @Test
    public void testGetJournalTitle() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser09254005_165_1();
        assertEquals("Sensors and Actuators B: Chemical", parser.getJournalTitle());
    }

    @Test
    public void testGetVolume() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser09254005_165_1();
        assertEquals("165", parser.getVolume());
    }

    @Test
    public void testGetNumber() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser09254005_165_1();
        assertEquals("1", parser.getNumber());
    }

    @Test
    public void testGetNumberWhenMissing() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser01677322_168();
        assertEquals(Issue.NULL_NUMBER, parser.getNumber());
    }

    @Test
    public void testGetArticleNodes() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser09254005_165_1();
        assertEquals(26, parser.getArticleNodes().size());
    }
    
    @Test
    public void testGetArticleTitle() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser09254005_165_1();
        List<Node> nodes = parser.getArticleNodes();
        assertEquals("A nanostructured SAW chip-based biosensor detecting cancer cells", parser.getArticleTitle(nodes.get(1)));
    }

    @Test
    public void testGetArticleAuthors() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser09254005_165_1();
        List<Node> nodes = parser.getArticleNodes();
        assertEquals(asList("Patrick Br\u00f6ker", "Klaus L\u00fccke", "Markus Perpeet", "Thomas M.A. Gronewold"), parser.getArticleAuthors(nodes.get(1)));
    }

    @Test
    public void testGetArticlePages() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser09254005_165_1();
        List<Node> nodes = parser.getArticleNodes();
        assertEquals("1-6", parser.findArticlePages(nodes.get(1)));
    }

    @Test
    public void testGetArticleUrl() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser09254005_165_1();
        List<Node> nodes = parser.getArticleNodes();
        assertEquals(URI.create("http://www.sciencedirect.com/science/article/pii/S0925400511010197"), parser.getArticleUrl(nodes.get(1)));
    }

    @Test
    public void testGetArticleId() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser09254005_165_1();
        List<Node> nodes = parser.getArticleNodes();
        assertEquals(new ArticleId(JOURNAL_09254005, "S0925400511010197"), parser.getArticleId(nodes.get(1)));
    }

    @Test
    public void testGetPreviousIssueLink() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser09254005_165_1();
        IssueLink issueRef = parser.getPreviousIssue();
        assertEquals("164", issueRef.getVolume());
        assertEquals("1", issueRef.getNumber());
        assertEquals(URI.create("http://www.sciencedirect.com/science/journal/09254005/164/1"), issueRef.getUrl());
    }

    @Test
    public void testGetPreviousIssueLinkWithoutNumber() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser01677322_168();
        IssueLink issueRef = parser.getPreviousIssue();
        assertEquals("167", issueRef.getVolume());
        assertEquals(Issue.NULL_NUMBER, issueRef.getNumber());
        assertEquals(URI.create("http://www.sciencedirect.com/science/journal/01677322/167"), issueRef.getUrl());
    }

    @Test
    public void testGetPreviousIssueLinkWithSupplement() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser00928674_116_1();
        IssueLink issueRef = parser.getPreviousIssue();
        assertEquals("116", issueRef.getVolume());
        assertEquals("(S2)", issueRef.getNumber());
        assertEquals(URI.create("http://www.sciencedirect.com/science/journal/00928674/116/supp/S2"), issueRef.getUrl());
    }

    @Test
    public void testSupplementNumber() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser00928674_116_S2();
        assertEquals("S2", parser.getNumber());
    }

    @Test
    public void testSupplementDate() throws Exception {
        ElsevierIssueTocParser parser = getJournalParser00928674_116_S2();
        assertEquals("2004", parser.getYear());
    }

    private ElsevierIssueTocParser getJournalParser00928674_116_1() throws Exception {
        Document journal = journal000928674;
        if (journal == null) {
            journal = loadHtml("00928674-116-1.html");
            journal000928674 = journal;
        }
        return new ElsevierIssueTocParser(journal, URI.create("http://www.sciencedirect.com/science/journal/00928674/116/1"), JOURNAL_09254005);
    }

    private ElsevierIssueTocParser getJournalParser00928674_116_S2() throws Exception {
        Document journal = journal000928674_S2;
        if (journal == null) {
            journal = loadHtml("00928674-116-S2.html");
            journal000928674_S2 = journal;
        }
        return new ElsevierIssueTocParser(journal, URI.create("http://www.sciencedirect.com/science/journal/00928674/116/supp/S2"), JOURNAL_09254005);
    }

    private ElsevierIssueTocParser getJournalParser09254005_165_1() throws Exception {
        Document journal = journal09254005;
        if (journal == null) {
            journal = loadHtml("09254005-165-1.html");
            journal09254005 = journal;
        }
        return new ElsevierIssueTocParser(journal, URI.create("http://www.sciencedirect.com/science/journal/09254005/165/1"), JOURNAL_09254005);
    }

    private ElsevierIssueTocParser getJournalParser01677322_168() throws Exception {
        Document journal = journal01677322;
        if (journal == null) {
            journal = loadHtml("01677322-168.html");
            journal01677322 = journal;
        }
        return new ElsevierIssueTocParser(journal, URI.create("http://www.sciencedirect.com/science/journal/01677322/168"), JOURNAL_09254005);
    }

    private Document loadHtml(final String filename) throws Exception {
        Builder builder = new Builder(new Parser());
        InputStream in = getClass().getResourceAsStream(filename);
        try {
            return builder.build(new InputStreamReader(in, "UTF-8"));
        } finally {
            IOUtils.closeQuietly(in);
        }
    }

}
