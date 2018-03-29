#include <optparser>

#include <iomanip>

int
main(int argc, const char** argv)
{
    OptionParser::Arguments args;
    OptionParser::Options opts;

    OptionParser optParser(&args, &opts);
    optParser.setNArguments(3, 3);

    optParser.addUsage("arg1 arg2 arg3");
    optParser.addDescription("Britten's other works range from orchestral to choral, solo vocal, chamber and instrumental as well as film music. He took a great interest in writing music for children and amateur performers. He often composed with particular performers in mind. His most frequent and important muse was his personal and professional partner, the tenor Peter Pears; others included Janet Baker, Dennis Brain, Julian Bream, Dietrich Fischer-Dieskau and Mstislav Rostropovich. Britten was a celebrated pianist and conductor, performing many of his own works in concert and on record. He also performed and recorded works by others, such as Bach's Brandenburg concertos, Mozart symphonies, and song cycles by Schubert and Schumann.");
    optParser.addCopyright("2012 by Albert Einstein");
    optParser.addOption("option1", "-a", "F", "--option-1", "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur massa dui, malesuada in aliquam a, accumsan in odio. Mauris pulvinar, ante ac auctor pellentesque, purus nibh posuere risus, vel consectetur magna ipsum non nibh.");
    optParser.addOption("option2", "-b", "F", "--option-2", "This option has a default value [default = \"%default\"]", "howdy");
    optParser.addSection("This is a section");
    optParser.addFlag("flag1", "-r", "--a-flag", "An amazing flag.");

    optParser.addFlag("storeTrue", "-t", "--store-true", "An amazing flag.", true, false);
    optParser.addFlag("storeFalse", "-f", "--a-very-very-very-very-very-long-flag", "Duis dictum arcu ut ligula varius congue. Morbi posuere tempor erat, eget vestibulum tellus facilisis vel. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Duis semper eget mauris eu varius. Praesent vel lorem nulla. Nam sollicitudin turpis a elit aliquam scelerisque.", false, true);

    optParser.parse(argc, argv);

    std::cout << "Values for options" << std::endl;
    for(std::map<std::string, OptionParser::ConvertibleString>::iterator opt = opts.begin(); opt != opts.end(); opt++) {
        std::cout << std::setw(10) << opt->first << " = " << opt->second << "\n";
    }
}

