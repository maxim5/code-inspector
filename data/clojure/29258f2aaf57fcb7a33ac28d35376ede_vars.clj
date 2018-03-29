(load-file "src/clojure/sup.clj")

(load-data)

;; how many rated lines have we got?
(->> dv (map :rate) (filter #(= 1 %)) count)
;;  => 211

;; vector of id-user's
(def users (map :user dv))

;; group users with similar demographics together
(def umap (group-users users))
;; 
(def maybe-same-users (filter (fn [[user groups]] (> (count groups) 1)) umap))
(count maybe-same-users)
;; 494

;; explore the potentially same or similar users
;; under the same demographics key, we show, for each site, what lines the demographics is encountered in
(take 5 maybe-same-users)
;; ([{:country "USA", :state "New York", :city "New York", :age 22, :gender 1} {1 [418 897], 0 [442]}] [{:country "USA", :state "California", :city "Palo Alto", :age 62, :gender 1} {3 [624], 0 [509]}] [{:country "USA", :state "Pennsylvania", :city "Norristown", :age 35, :gender 1} {3 [987], 0 [962]}] [{:country "USA", :state "Texas", :city "Austin", :age 23, :gender 1} {4 [637], 3 [883], 0 [843]}] [{:country "USA", :state "Texas", :city "Austin", :age 55, :gender 1} {3 [110], 1 [408]}])


;; convert all categorical data into numbers
;; the numbers are assigned as ids, consecutively from 0 on, within each data class, on a first seen, first id'ed basis
;; numeric-data uses the global closures with counters defined in sup.clj, explored later 
(def dn (map numeric-data dv))

;; show the top 10 states by count, i.e. which states most data points are from
(top-maps *state-map* 10)
;; (["California" 521] ["Texas" 252] ["New York" 220] ["Florida" 192] ["Illinois" 144] ["Pennsylvania" 121] ["Georgia" 115] ["Washington" 112] ["Virginia" 106] ["Ohio" 105])

;;  top 10 countries
(top-maps *country-map* 10)
;; (["USA" 3278] ["UK" 279] ["Canada" 169] ["Australia" 86] ["India" 66] ["Germany" 59] ["Turkey" 38] ["Netherlands" 32] ["Italy" 30] ["New Zealand" 26])

;; top 10 cities
(top-maps *city-map* 10)
;; (["Los Angeles" 57] ["New York" 47] ["Seattle" 42] ["Atlanta" 40] ["Houston" 39] ["Austin" 39] ["Denver" 37] ["Chicago" 35] ["San Francisco" 30] ["San Diego" 29])

;; top 10 tags
(top-maps *tag-map* 100)
;; ([:humor 3173] [:bizarre 2835] [:internet 2822] [:photography 2741] [:science 2649] [:online-games 2265] [:satire 2218] [:cyberculture 2119] [:arts 2086] [:graphic-design 1998] [:software 1942] [:animation 1891] [:multimedia 1879] [:web-design 1861] [:open-source 1818] [:cooking 1784] [:astronomy 1774] [:computer-graphics 1768] [:self-improvement 1761] [:video-games 1703] [:music 1680] [:ecommerce 1670] [:windows 1664] [:travel 1657] [:illusions 1633] [:computers 1596] [:investing 1592] [:books 1540] [:drawing 1539] [:computer-hardware 1506] [:linguistics 1487] [:movies 1474] [:psychology 1426] [:philosophy 1420] [:adult-humor 1404] [:alternative-news 1384] [:history 1368] [:network-security 1351] [:firefox 1345] [:business 1339] [:cartoons 1334] [:physics 1320] [:politics 1296] [:programming 1293] [:geography 1292] [:computer-science 1291] [:blogs 1288] [:writing 1287] [:video 1235] [:hacking 1219] [:science-fiction 1216] [:internet-tools 1199] [:linux 1197] [:tv 1188] [:activism 1173] [:space-exploration 1167] [:journalism 1119] [:entrepreneurship 1103] [:relationships 1103] [:environment 1101] [:card-games 1101] [:fine-arts 1101] [:research 1099] [:nude-art 1082] [:liberties 1071] [:news 1071] [:mathematics 1069] [:electronics 1059] [:american-history 1056] [:comics 1045] [:stumblers 1031] [:literature 1025] [:shareware 1011] [:logic 1009] [:health 1000] [:clothing 992] [:porn 977] [:consumer-info 971] [:operating-systems 954] [:cognitive-science 953] [:complex-systems 950] [:indie-film 948] [:alcohol 925] [:ancient-history 916] [:mythology 908] [:painting 906] [:sexuality 890] [:audio 870] [:advertising 867] [:library-resources 863] [:video-equipment 854] [:aerospace 853] [:ai 850] [:architecture 849] [:iraq 846] [:conspiracies 831] [:christianity 828] [:animals 828] [:desktop-publishing 819] [:peripheral-devices 808])

;; study the time range
(def dt (sort (map :time dv)))
[(first dt) (last dt)]
;; [#<DateTime 2006-03-28T00:25:16.000-05:00> #<DateTime 2006-06-05T23:06:51.000-04:00>]

;; gender studies with politically incorrect results
(count (filter #(= (:gender %) 1) users))
;; 4032 men
(count (filter #(= (:gender %) 2) users))
;; 453 women

;; how many different ratings of each kind
(count (filter #(= (:rate %) 0) dv))
;; 4164 unrated
(count (filter #(= (:rate %) 1) dv))
;; 211 thumbs up
(count (filter #(= (:rate %) -1) dv))
;; 110 thumbs down

;; how many different tags
(def *all-tags* (tag-simple-set dv))
;; (count *all-tags*)
;; 21490

;; how many unique tags the women got
(def womens-tags (uniq-tags dv #(= (:gender (:user %)) 2)))
;; (count womens-tags)
;; 2740

;; how many unique tags the men got
(def mens-tags (uniq-tags dv #(= (:gender (:user %)) 1)))
;; (count mens-tags)
;; 16707

;; get the top women-only tags used by the most distinct women
(def womens-tags-counted (uniq-tags-counted dv #(= (:gender (:user %)) 2)))
(def womens-tags-sorted (sort-by second > womens-tags-counted))
(take 50 womens-tags-sorted)
;; this is good!
;; ([:fabric 5] [:beads 5] [:gender-studies 5] [:hebrew 4] [:telecomunications 4] [:asl 4] [:angels 4] [:valentines-day 4] [:clip-art 4] [:handbags 4] [:emotional-well-being 4] [:candles 4] [:jewish-humor 4] [:eating-disorders 4] [:puppies 3] [:judaism-women 3] [:time-zone 3] [:world-war-ii 3] [:childcare 3] [:nanny-agencies 3] [:user-interface-design 3] [:stjordal 3] [:ragaee 3] [:sass 3] [:gifs 3] [:jewish-cooking 3] [:haiku 3] [:webcamera 3] [:lifestyles 3] [:rap-music 3] [:arab 3] [:omar-sharif 3] [:shoah 3] [:home-and-life 3] [:kotel 3] [:tzinus 3] [:stationary 3] [:jerusalem 3] [:the-righteous 3] [:auschwitz 3] [:radiohead 3] [:jewish-philosophy 3] [:detox 3] [:birthright 3] [:dreamweaver 3] [:frum 3] [:indie-pop 3] [:collectible-gifts 3] [:deaf-socitey 3] [:educational-helps 3])

;; get the top men-only tags used by the most distinct men
(def mens-tags-counted (uniq-tags-counted dv #(= (:gender (:user %)) 1)))
(take 50 mens-tags-sorted)
;; this is too good!
;; ([:pornography 53] [:download 24] [:digital 22] [:career 22] [:c 19] [:code 19] [:wireless 19] [:daily 19] [:gps 18] [:xp 18] [:sound 18] [:website 17] [:camera 15] [:hack 15] [:unix 14] [:star-trek 14] [:nasa 13] [:list 13] [:california 13] [:robots 13] [:tool 12] [:stock 12] [:housing 12] [:paper 12] [:sms 12] [:squash 12] [:innovation 12] [:lies 12] [:hosting 12] [:amazon 12] [:lego 12] [:psp 12] [:atheist-agnostic 12] [:pc 12] [:london 11] [:solar 11] [:nanotechnology 11] [:auto 11] [:testing 11] [:image 11] [:ruby 11] [:hot 11] [:password 11] [:guide 11] [:im 11] [:osx 10] [:democracy 10] [:encyclopedia 10] [:bittorrent 10] [:editor 10])

;; get the unique tags associated with the thumbs-up ratings
(def rate1-tags-counted (uniq-tags-counted dv #(= (:rate %) 1)))
(def rate1-tags-sorted (sort-by second > rate1-tags-counted)) 
(count rate1-tags-sorted)
;; 1249
(take 50 rate1-tags-sorted)
;; ([:criticism 3] [:picnic 3] [:user 3] [:going-places 2] [:harddrive 2] [:what-cats-do 2] [:zabbix 2] [:philamthropy 2] [:pdfs 2] [:huge 2] [:american-red-cross 2] [:pet-portraits-pastels 2] [:crowd 2] [:excellent-news-site 2] [:addon 2] [:streamer 2] [:minature 2] [:albumart 2] [:rsd 2] [:infra-red 2] [:raising 2] [:carputer 2] [:mitch-hedberg 2] [:pendrive 2] [:red-cross-heroes 2] [:lighters 2] [:zefrank 2] [:datamining 2] [:excellent-modern-artist 2] [:maker 2] [:nice-news-info 2] [:dead-sea-scrolls 2] [:wheels 2] [:kitty-computer-accessory 2] [:wardriving 2] [:de-crapify 2] [:fund 2] [:putt 2] [:optimism 2] [:decrpter 2] [:solar-storms 2] [:quirky-view 2] [:san 2] [:auto-dry 2] [:ja 2] [:remember 2] [:opinionated-site 2] [:famine 2] [:nice-widget 2] [:vacuformer 2])

;; get the uniqe tags associated with the thumbs-down ratings
(def rate-1-tags-counted (uniq-tags-counted dv #(= (:rate %) -1)))
(def rate-1-tags-sorted (sort-by second > rate-1-tags-counted))
(take 50 rate-1-tags-sorted)
;; ([:prague 2] [:message-board 2] [:smoker-circuit 1] [:aircraft-photos 1] [:film-festivals 1] [:book-stores 1] [:m-self 1] [:advertising-blog 1] [:ageless-wisdom 1] [:houseboat-summit-watts 1] [:american-dream 1] [:salsa 1] [:tbn 1] [:yours-truly 1] [:teenage-repellant 1] [:sexual-diseases 1] [:graphics-astronomy 1] [:chick-flicks 1] [:levers 1] [:artist-graphic-comics 1] [:culinary 1] [:design-magazine 1] [:pastafarianism 1] [:bd 1] [:eurovision 1] [:amazing-aircraft-photos 1] [:dance-mix 1] [:devil-s-story 1] [:video-art 1] [:cultural-studies 1] [:pythpn 1] [:disk-arknoid 1] [:zizek 1] [:vehicles 1] [:foreign-film 1] [:ex-yugoslavia 1] [:ball-toucher 1] [:draw-picasso-head 1] [:honorable 1] [:game-synthesis 1] [:english-mistakes 1] [:fishy-propaganda 1] [:looking-glass 1] [:film-journals 1] [:myth-making 1] [:quantum-news 1] [:hogwash 1] [:ich-ag 1] [:here-kitty-kitty 1] [:awesome-color-mountain 1])

;; perform kmeans-clustering of the tags into 10 clusters, 10 iterations, and show the tag centroids for each cluster
(time (def km (kmeans tags 10 10)))
(print-km-tops km)
;; [0 144 ([:stumblers 642] [:politics 341] [:science 219] [:health 198] [:bizarre 177] [:h5n1 127] [:liberties 103] [:physics 101] [:humor 101] [:iraq 95])]
;; [1 1493 ([:photography 1346] [:humor 810] [:graphic-design 673] [:internet 572] [:arts 448] [:politics 424] [:science 368] [:bizarre 355] [:video-games 322] [:web-design 302])]
;; [2 798 ([:humor 1228] [:system-imported 898] [:nude-art 353] [:horses 291] [:adult-humor 272] [:bizarre 263] [:horse 193] [:stumblers 161] [:windows 145] [:martial-arts 139])]
;; [3 64 ([:politics 4130] [:blogs 1652] [:iraq 1209] [:military 888] [:crime 885] [:science 740] [:internet 706] [:middle-east 653] [:activism 640] [:computer-hardware 605])]
;; [4 164 ([:stumblers 8565] [:humor 1349] [:internet 1054] [:photography 1021] [:bizarre 925] [:nude-art 874] [:video-games 797] [:painting 669] [:blogs 638] [:drugs 623])]
;; [5 407 ([:photography 3657] [:arts 1124] [:stumblers 1018] [:married-life 786] [:military 684] [:american-history 501] [:nude-art 455] [:painting 414] [:history 408] [:humor 361])]
;; [6 419 ([:stumblers 3452] [:open-source 2483] [:politics 1869] [:internet 1709] [:iraq 1366] [:web-design 1266] [:video-games 1260] [:music 1157] [:programming 1107] [:humor 1088])]
;; [7 298 ([:porn 1205] [:nude-art 712] [:humor 520] [:stumblers 439] [:adult-humor 413] [:gay-sex 280] [:photography 219] [:blogs 212] [:linux 203] [:roleplaying-games 192])]
;; [8 586 ([:environment 165] [:cats 160] [:cooking 147] [:christianity 145] [:ecommerce 127] [:amateur-radio 121] [:self-improvement 120] [:bizarre 108] [:graphic-design 104] [:activism 103])]
;; [9 112 ([:astronomy 569] [:unfiled 524] [:catholic 433] [:physics 378] [:linux 360] [:open-source 353] [:space-exploration 319] [:science 188] [:politics 119] [:internet 108])]

;; some Incanter charting
;; see http://incanter.org/ for Incanter info
;; bar chart of the number of data points from a state by state, in descending order by state
;;  California is leftmost with 521
(require 'incanter.charts)
(def states-desc (->> ((*state-map* :get-map)) (map (fn [[k v]] [k (count (second v))])) (sort-by second >)))
(view (incanter.charts/bar-chart (map first states-desc) (map second states-desc)))
