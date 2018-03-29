package wwmm.pubcrawler.crawlers.elsevier.parsers;

import wwmm.pubcrawler.model.Issue;
import wwmm.pubcrawler.model.IssueLink;
import wwmm.pubcrawler.model.IssueLinkBuilder;
import wwmm.pubcrawler.model.id.JournalId;

import java.net.URI;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Sam Adams
 */
public class ElsevierIssueLinkParser {

    // /science/journal/09254005/164
    private static final Pattern VOLUME =
     Pattern.compile("/science/journal/([^/]+)/([^/]+)");

    // /science/journal/09254005/164/1
    private static final Pattern VOLUME_ISSUE =
     Pattern.compile("/science/journal/([^/]+)/([^/]+)/([^/]+)");

    // /science/journal/00012092/94/6/supp/S
    // /science/journal/09600760/39/5/part/P2
    private static final Pattern VOLUME_ISSUE_SUPPLEMENT =
     Pattern.compile("/science/journal/([^/]+)/([^/]+)/([^/]+)/(?:supp|part)/([^/]+)");

    // /science/journal/00457949/62/part/P1
    // /science/journal/03770427/71-80/index/I1
    private static final Pattern VOLUME_PART =
     Pattern.compile("/science/journal/([^/]+)/([^/]+)/(?:index|part|supp)/([^/]+)");


    public IssueLink parseIssueLink(final JournalId journalId, final String journalTitle, final URI url, final String link) {
        final IssueLinkBuilder builder = new IssueLinkBuilder()
                .withJournalId(journalId)
                .withJournalTitle(journalTitle)
                .withUrl(url);
        return parseVolumeIssueLink(link, builder);
    }

    private IssueLink createIssueLink(final IssueLinkBuilder builder, final String volume, final String number) {
        return builder
                .withVolume(volume)
                .withNumber(number)
                .build();
    }

    private IssueLink parseVolumeIssueLink(final String link, final IssueLinkBuilder builder) {
        final Matcher matcher = VOLUME_ISSUE.matcher(link);
        if (matcher.matches()) {
            final String volume = matcher.group(2);
            final String number = matcher.group(3);
            return createIssueLink(builder, volume, number);
        }
        return parseIssueSupplementLink(link, builder);
    }

    private IssueLink parseIssueSupplementLink(final String link, final IssueLinkBuilder builder) {
        final Matcher matcher = VOLUME_ISSUE_SUPPLEMENT.matcher(link);
        if (matcher.matches()) {
            final String volume = matcher.group(2);
            final String number = matcher.group(3);
            final String supplement = matcher.group(4);
            return createIssueLink(builder, volume, number + '(' + supplement + ')');
        }
        return parseVolumePartLink(link, builder);
    }

    private IssueLink parseVolumePartLink(final String link, final IssueLinkBuilder builder) {
        final Matcher matcher = VOLUME_PART.matcher(link);
        if (matcher.matches()) {
            final String volume = matcher.group(2);
            final String part = matcher.group(3);
            return createIssueLink(builder, volume, '(' + part + ')');
        }
        return parseVolumeLink(link, builder);
    }

    private IssueLink parseVolumeLink(final String link, final IssueLinkBuilder builder) {
        final Matcher matcher = VOLUME.matcher(link);
        if (matcher.matches()) {
            final String volume = matcher.group(2);
            return createIssueLink(builder, volume, Issue.NULL_NUMBER);
        }
        return null;
    }

}
