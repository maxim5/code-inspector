package wwmm.pubcrawler.crawlers.elsevier.parsers;

import org.junit.Test;
import wwmm.pubcrawler.model.id.IssueId;
import wwmm.pubcrawler.model.id.JournalId;

import java.net.URI;

import static org.junit.Assert.assertEquals;

/**
 * @author Sam Adams
 */
public class ElsevierIssueLinkParserTest {

    private static final JournalId JOURNAL = new JournalId("journal");

    private ElsevierIssueLinkParser parser = new ElsevierIssueLinkParser();

    @Test
    public void testParseVolumeIssueLink() {
        assertEquals(new IssueId(JOURNAL, "164", "1"), parser.parseIssueLink(JOURNAL, "TITLE", URI.create("http://foo.com/"), "/science/journal/09254005/164/1").getIssueId());
    }

    @Test
    public void testParseVolumeIssueSupplementLink() {
        assertEquals(new IssueId(JOURNAL, "94", "6(S)"), parser.parseIssueLink(JOURNAL, "TITLE", URI.create("http://foo.com/"), "/science/journal/00012092/94/6/supp/S").getIssueId());
    }

    @Test
    public void testParseVolumeIssuePartLink() {
        assertEquals(new IssueId(JOURNAL, "39", "5(P2)"), parser.parseIssueLink(JOURNAL, "TITLE", URI.create("http://foo.com/"), "/science/journal/09600760/39/5/part/P2").getIssueId());
    }

    @Test
    public void testParseVolumePartLink() {
        assertEquals(new IssueId(JOURNAL, "62", "(P1)"), parser.parseIssueLink(JOURNAL, "TITLE", URI.create("http://foo.com/"), "/science/journal/00457949/62/part/P1").getIssueId());
    }

    @Test
    public void testParseVolumeIndexLink() {
        assertEquals(new IssueId(JOURNAL, "71-80", "(I1)"), parser.parseIssueLink(JOURNAL, "TITLE", URI.create("http://foo.com/"), "/science/journal/03770427/71-80/index/I1").getIssueId());
    }

    @Test
    public void testParseVolumeSuppLink() {
        assertEquals(new IssueId(JOURNAL, "116", "(S2)"), parser.parseIssueLink(JOURNAL, "TITLE", URI.create("http://foo.com/"), "/science/journal/00928674/116/supp/S2").getIssueId());
    }

    @Test
    public void testParseVolumeLink() {
        assertEquals(new IssueId(JOURNAL, "93", "-"), parser.parseIssueLink(JOURNAL, "TITLE", URI.create("http://foo.com/"), "/science/journal/00399140/93").getIssueId());
    }
}
