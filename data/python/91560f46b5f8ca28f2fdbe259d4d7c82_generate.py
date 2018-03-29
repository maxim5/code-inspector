#!/usr/bin/python
import simplejson

TAGS = [
    ("Development status", "Development status of the software",
     [("not ready for use", "In development, but not ready for use"),
      ("in development", "In development, partially usable"),
      ("mature", "Mature and usable"),
      ("obsolete", "Obsoleted software")
      ]),
    ("Quality", "Quality of the software",
     [("reliable",
       "Results from this software have been verified extensively at "
       "least by some people independently and found valid"),
      ("contains errors", "The software is known to contain errors or "
       "produce invalid results (in cases which it claims to handle)"),
      ("incomplete", "The software solves only a subset of the problems it "
       "claims to handle")]),
    ("Environment", "Environment where the software operates",
     [("console application", "Application running in the console"),
      ("web application", "Application running in the web server"),
      ("GUI application", "Application with a graphical user interface"),
      ("mobile application", "Application running primarily on mobile devices"),
      ("daemon application", "Application running primarily in the background"),
      ("library", "Application library")]),
    ("Field", "Main (scientific) field of the software, if any",
     [("astronomy", "Astronomy"),
      ("physics", "Physics"),
      ("chemistry", "Chemistry"),
      ("earth science", "Earth science"),
      ("environmental science", "Environmental science"),
      ("biology", "Biology"),
      ("cognitive science", "Cognitive science"),
      ("computer science", "Computer science"),
      ("mathematics", "Mathematics"),
      ("statistics", "Statistics"),
      ("systems science", "Systems science"),
      ("anthropology", "Anthropology"),
      ("economics", "Economics"),
      ("linguistics", "Linguistics"),
      ("psychology", "Psychology"),
      ("geography", "Geography"),
      ("philosophy", "Philosophy"),
      ("political science", "Political science"),
      ("sociology", "Sociology"),
      ("history", "History"),
      ("agronomy", "Agronomy"),
      ("architecture", "Architecture"),
      ("education", "Education"),
      ("engineering", "Engineering"),
      ("medical science", "Medical science"),
      ("management", "Management"),
      ("military science", "Military science"),
      ("spatial science", "Spatial science (GIS, remote sensing, etc.)"),
      ]),
    ("Topic", "Topic of the software",
     [("speech processing", "Speech processing"),
      ("audio capture", "Audio capture"),
      ("audio processing", "Audio processing"),
      ("video capture", "Video capture"),
      ("video processing", "Video processing"),
      ("image capture", "Image capture"),
      ("image processing", "Image processing"),
      ("image recognition", "Image recognition"),
      ("3d modeling", "3D modeling"),
      ("3d rendering", "3D rendering"),
      ("visualization", "Data visualization"),
      ("database", "Database-related software"),
      ("graphics", "Graphics-related software"),
      ("text indexing", "Text indexing related software"),
      ("optimization", "Optimization"),
      ("integration", "Integration"),
      ("PDE (partial differential equations)",
       "Partial differential equations"),
      ("dynamical systems", "Dynamical systems"),
      ("atmospheric science", "Atmospheric science"),
      ("bio-informatics", "Bioinformatics"),
      ("genealogy", "Genealogy"),
      ("cryptography", "Cryptography"),
      ("computer vision", "Computer vision"),
      ("artificial intelligence", "Artificial intelligence"),
      ("artificial life", "Artificial life"),
      ("GIS (geographic information systems)",
       "Geographic information systems"),
      ("machine learning", "Machine learning"),
      ("electronic design automation", "Electronic design automation"),
      ("human machine interfaces", "Human machine interfaces"),
      ("security", "Security-related software"),
      ]),
]

def dump_tags(items):
    pk_tag = 1
    pk_cat = 1
    for cat_name, cat_desc, tags in TAGS:
        cat_entry = dict(model="community.tagcategory",
                         pk=pk_cat,
                         fields=dict(name=cat_name, description=cat_desc))
        items.append(cat_entry)
        for tag_name, tag_desc in tags:
            tag_entry = dict(model="community.tag",
                             pk=pk_tag,
                             fields=dict(category=pk_cat,
                                         name=tag_name,
                                         description=tag_desc))
            items.append(tag_entry)
            pk_tag += 1
        pk_cat += 1

if __name__ == "__main__":
    items = []
    dump_tags(items)

    f = open('sample.json', 'w')
    simplejson.dump(items, f)
    f.close()
