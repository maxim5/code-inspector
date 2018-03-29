(ns wwmm.pubcrawler.actacryst
	(:use wwmm.pubcrawler.core))

(defn get-actacryst-issue-url
	"Gets the URL of an Acta Cryst. journal with the provided year and issue."
	([journal]
		(str "http://journals.iucr.org/" (:abbreviation (acta-journals journal)) "/contents/backissuesbdy.html"))
		
	([journal year issue]
		(let [jrnl (acta-journals journal)
					issue-num (if (> 10 issue) (str "0" issue) (str issue))]
			(str "http://journals.iucr.org/" (:abbreviation jrnl) "/issues/" year "/" issue-num "/00/isscontsbdy.html"))))

(def actacryst-journals
		{:section-a (struct journal "a" "Section A: Foundations of Crystallography" 0)
		:section-b (struct journal "b" "Section B: Structural Science" 0)
		:section-c (struct journal "c" "Section C: Crystal Structure Communications" 0)
		:section-d (struct journal "d" "Section D: Biological Crystallography" 0)
		:section-e (struct journal "e" "Section E: Structure Reports" 0)
		:section-f (struct journal "f" "Section F: Structural Biology and Crystallization Communications" 0)
		:section-j (struct journal "j" "Section J: Applied Crystallography" 0)
		:section-s (struct journal "s" "Section S: Synchrotron Radiation" 0)})