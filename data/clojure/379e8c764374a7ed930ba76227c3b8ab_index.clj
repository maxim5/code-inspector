(ns it354.views.index
  (:use [noir.core :only [defpage defpartial]]
        [hiccup.core :only [html]])
  (:require [it354.views.layout :as layout]
            [it354.views.utils :as utils]
            [it354.models.db :as db]
            [noir.response :as response]))

(defpage "/" []
  (layout/layout "Brian Jesse"
    [:div.wrapper
      [:div.title.hidden-phone
        [:div.container
          [:div.row
            [:div.span2.hidden-phone
              [:img.img-polaroid {:src (utils/add-context "/img/me.jpg")}]]
            [:div.span10
              [:div.row
                [:h1 "Brian Jesse"]]
              [:div.row
                [:a.btn.btn-primary.btn-large {:href "https://www.facebook.com/BJesse90"} 
                  [:i.icon-facebook-sign.icon-large] 
                  "View Facebook Profile"]
                [:a.btn.btn-primary.btn-large {:href "http://www.linkedin.com/pub/brian-jesse/57/769/136"} 
                  [:i.icon-linkedin-sign.icon-large] 
                  "View Linkedin Profile"]]]]]]
      [:div#main_content.content
        [:div.navbar.navbar-fixed-top.visible-phone
          [:div.navbar-inner
            [:div.container
              [:a.btn.btn-navbar {:data-toggle "collapse" :data-target ".nav-collapse"}
                [:span.icon-reorder]]
              [:a.brand {:href "#"} "Brian Jesse"]
              [:div.nav-collapse
                [:ul.nav
                  [:li [:a {:href "http://www.linkedin.com/pub/brian-jesse/57/769/136"} [:i.icon-linkedin-sign.pull-right] "LinkedIn"]]
                  [:li [:a {:href "https://www.facebook.com/BJesse90"} [:i.icon-facebook-sign.pull-right] "Facebook"]]
                  [:li [:a {:href "#/home"} [:i.icon-chevron-right.pull-right] "Home"]]
                  [:li [:a {:href "#/resume"} [:i.icon-chevron-right.pull-right] "Resume"]]
                  [:li [:a {:href "#/links"} [:i.icon-chevron-right.pull-right] "Links"]]
                  [:li [:a {:href "#/pipes"} [:i.icon-chevron-right.pull-right] "Pipes"]]
                  [:li [:a {:href "#/restaurants"} [:i.icon-chevron-right.pull-right] "Restaurants"]]
                  [:li [:a {:href "#/places"} [:i.icon-chevron-right.pull-right] "Places"]]
                  [:li [:a {:href "#/examples"} [:i.icon-chevron-right.pull-right] "Examples"]]
                  [:li [:a {:href "#/gallery"} [:i.icon-chevron-right.pull-right] "Gallery"]]
                  [:li [:a {:href "#/videos"} [:i.icon-chevron-right.pull-right] "Videos"]]]]]]]
        [:div.container
          [:div.row
            [:div.span3.hidden-phone
              [:ul.nav.nav-list
                [:li [:a {:href "#/home" :class "{{homeLink}}"} [:i.icon-chevron-right] "Home"]]
                [:li [:a {:href "#/resume" :class "{{resumeLink}}"} [:i.icon-chevron-right] "Resume"]]
                [:li [:a {:href "#/links" :class "{{linksLink}}"} [:i.icon-chevron-right] "Links"]]
                [:li [:a {:href "#/pipes" :class "{{pipesLink}}"} [:i.icon-chevron-right] "Pipes"]]
                [:li [:a {:href "#/restaurants" :class "{{restaurantsLink}}"} [:i.icon-chevron-right] "Restaurants"]]
                [:li [:a {:href "#/places" :class "{{placesLink}}"} [:i.icon-chevron-right] "Places"]]
                [:li [:a {:href "#/examples" :class "{{examplesLink}}"} [:i.icon-chevron-right] "Examples"]]
                [:li [:a {:href "#/gallery" :class "{{galleryLink}}"} [:i.icon-chevron-right] "Gallery"]]
                [:li [:a {:href "#/videos" :class "{{videosLink}}"} [:i.icon-chevron-right] "Videos"]]]]
            [:div.span9
              [:div.well {:ng-view ""}]]]]]
      [:div.push]]
    [:footer
      [:div.container
        [:h4 "&copy; Brian Jesse 2012"]]]))

(defpage "/partials/home" []
  (html
    (utils/comment-html "Facebook Badge START")
    [:a.fb-badge {:href "https://www.facebook.com/BJesse90" :target "_TOP" :title "Brian Jesse"}
      [:img.img-rounded {:src "https://www.facebook.com/badge.php?id=1463500045&bid=2846&key=783105830&format=png&z=1373594599" :style "border: 0px;"}]]
    (utils/comment-html "Facebook Badge END")
    [:h2 "About Me"]
    [:p "I am currently pursuing a bachelor's degreen in Computer Science at Illinois State University. I work for the department of Business Intelligence and Technology Solutions at ISU as a Java web programmer. I have experience working in tech support in a small business and an enterprise environment. Last spring I was selected as a finalist in the State Farm Mobile App Development competition for my social goal tracking app Goalcharge"]
      [:p [:a.btn {:href "#/videos"} "Check out the Goalcharge video &raquo;"]]))

(defpage "/partials/resume" []
  (html
    [:h2 "Resume"]
      [:hr]
      [:h3 "Objective"]
      [:table.table.table-bordered.table-striped
        [:tr
          [:td "To enhance my web development expertise, further my programming skills, and improve my general computer knowledge in a challenging, fast paced, and team-oriented environment."]]]
      [:h3 "Education"]
      [:table.table.table-bordered.table-striped.table-hover
        [:thead
          [:tr
            [:th {:colspan "2"} [:strong "Illinois State University, "] "Normal IL (2010-Present)"]]]
        [:colgroup
          [:col.span2]
          [:col.span8]]
        [:tbody
          [:tr
            [:td 
              [:u "Major:"]]
            [:td "Computer Science"]]
          [:tr
            [:td 
              [:u "Curriculum:"]]
            [:td "Mathematics, technical and group skills, programming, and web development"]]
          [:tr
            [:td 
              [:u "Involvement:"]]
            [:td "IT club; performed as webmaster for various clubs"]]]]
      [:table.table.table-bordered.table-striped.table-hover
        [:thead
          [:tr
            [:th {:colspan "2"} [:strong "Illinois Valley Community College, "] "Oglesby IL (2008-2010)"]]]
        [:colgroup
          [:col.span2]
          [:col.span8]]
        [:tbody
          [:tr
            [:td 
              [:u "Curriculum:"]]
            [:td "Programming, Electronic Theory, Chemistry, Calculus"]]
          [:tr
            [:td 
              [:u "Involvement:"]]
            [:td "Association of Information Technology Professionals (AITP), Psychology Club, theater, political activism, and civic organizations"]]]]
      [:h3 "Qualifications"]
      [:table.table.table-bordered.table-striped.table-hover
        [:tr
          [:td "Two years of Java experience including use of JPA, JSF, and restful web services"]]
        [:tr
          [:td "Finalist in a mobile app development contest"]]
        [:tr
          [:td "Website design skills using HTML 5 and CSS 3 for desktops and mobile devices"]]
        [:tr
          [:td "Apache, Glassfish, PHP, and JavaScript development skills"]]
        [:tr
          [:td "Unix server administration on Debian and Red Hat installations"]]
        [:tr
          [:td "C++ proficiency and extensive knowledge of data structures and algorithms"]]
        [:tr
          [:td "Experience working with collaborative build tools such as Maven, Subversion, and Git"]]
        [:tr
          [:td "Talents with graphic arts and video editing utilizing Adobe suites"]]]
      [:h3 "Experience"]
      [:table.table.table-bordered.table-striped.table-hover
        [:thead
          [:tr
            [:th {:colspan "2"} [:strong "Business Intelligence and Technology Solutions - Illinois State University, "] "Normal IL"]]
          [:tr
            [:th {:colspan "2"}  "Web Team Intern (Aug 2011-Present)"]]]
        [:colgroup
          [:col.span2]
          [:col.span8]]
        [:tbody
          [:tr
            [:td 
              [:u "Duties:"]]
            [:td "Java web app programming and rewriting, CMS integration with website"]]
          [:tr
            [:td 
              [:u "Skills Gained:"]]
            [:td "Project management and development in a highly skilled environment, multiple development environments"]]]]
      [:table.table.table-bordered.table-striped.table-hover
        [:thead
          [:tr
            [:th {:colspan "2"} [:strong "Enterprise Systems Support - Illinois State University, "] "Normal IL"]]
          [:tr
            [:th {:colspan "2"}  "Senior Technician (Mar 2011 - Aug 2011)"]]]
        [:colgroup
          [:col.span2]
          [:col.span8]]
        [:tbody
          [:tr
            [:td 
              [:u "Duties:"]]
            [:td "IT support, secure computer deployment, mobile development, and supervising techs"]]
          [:tr
            [:td 
              [:u "Skills Gained:"]]
            [:td "Managing multiple open tickets, and solving customerâs issues"]]]]
      [:table.table.table-bordered.table-striped.table-hover
        [:thead
          [:tr
            [:th {:colspan "2"} [:strong "Computer Spa, "] "Ottawa IL"]]
          [:tr
            [:th {:colspan "2"}  "(Jun 2009 - May 2010)"]]]
        [:colgroup
          [:col.span2]
          [:col.span8]]
        [:tbody
          [:tr
            [:td 
              [:u "Duties:"]]
            [:td "Computer repair and maintenance, maintaining Windows and Novell networks"]]
          [:tr
            [:td 
              [:u "Skills Gained:"]]
            [:td "Personal interaction with business clients"]]]]
      [:a.btn.btn-primary.btn-large {:href "http://www.linkedin.com/pub/brian-jesse/57/769/136"} 
                  [:i.icon-linkedin-sign.icon-large] 
                  "View Linkedin Profile"]))

(defpage "/partials/links" []
  (html
    [:h2 "My Favorite Links"]
    [:div#links.accordion
      [:div.accordion-group
        [:div.accordion-heading
          [:a.accordion-toggle {:data-toggle "collapse" :data-parent "#links" :href "#links1"} "Blogs"]]
        [:div#links1.accordion-body.collapse.in
          [:div.accordion-inner 
            [:ul
              [:li [:a {:href "//theverge.com" :target "_blank"} "The Verge"]]
              [:li [:a {:href "//xkcd.com/" :target "_blank"} "XKCD"]]
              [:li [:a {:href "//blog.hash-of-codes.com" :target "_blank"} "Hash of Codes"]]]]]]
      [:div.accordion-group
        [:div.accordion-heading
          [:a.accordion-toggle {:data-toggle "collapse" :data-parent "#links" :href "#links2"} "Social Media"]]
        [:div#links2.accordion-body.collapse
          [:div.accordion-inner
            [:ul
              [:li [:a {:href "//reddit.com" :target "_blank"} "Reddit"]]
              [:li [:a {:href "//coderwall.com" :target "_blank"} "Coderwall"]]
              [:li [:a {:href "//news.ycombinator.org" :target "_blank"} "Hacker News"]]]]]]
      [:div.accordion-group
        [:div.accordion-heading
          [:a.accordion-toggle {:data-toggle "collapse" :data-parent "#links" :href "#links3"} "Programming"]]
        [:div#links3.accordion-body.collapse
          [:div.accordion-inner
            [:ul
              [:li [:a {:href "//bitbucket.org" :target "_blank"} "Bitbucket"]]
              [:li [:a {:href "//webnoir.org" :target "_blank"} "Noir"]]
              [:li [:a {:href "//twitter.github.com" :target "_blank"} "Twitter Bootstrap"]]]]]]]))

(defpage "/partials/pipes" []
  (html
    [:h2 "Yahoo Pipes Example"]
      [:div#pipe]
      [:p [:a.btn {:href "http://pipes.yahoo.com/pipes/pipe.info?_id=0857e00eb73ac6bc8c974b60f6c30e96"} "View Pipe on Yahoo &raquo;"]]))

(defpage "/partials/videos" []
  (html
    [:h2 "My Videos"]
    [:hr]
    [:h3 "Goalcharge"]
    [:p "An app I created for the Mobile App contest last semester"]
    [:iframe.visible-desktop {:width "640" :height "480" :src "//www.youtube.com/embed/v3RTXbi7g4o?rel=0" :frameborder "0" :allowfullscreen ""}]
    [:iframe.visible-tablet {:width "480" :height "360" :src "//www.youtube.com/embed/v3RTXbi7g4o?rel=0" :frameborder "0" :allowfullscreen ""}]
    [:a.btn.visible-phone {:href "http://youtu.be/v3RTXbi7g4o"} "View Video On Youtube"]))

(defpage "/partials/places" []
  (html
    [:h2 "Places"]
    [:ul#places_tabs.nav.nav-tabs
      [:li.active
        [:a {:href "#" :ng-click "showNormal()"} "Normal"]]
      [:li
        [:a {:href "#" :ng-click "showChicago()"} "Chicago"]]]
    [:div#map_canvas]))

(defpage "/partials/places/normal" []
  (html
    [:div.info-window
      [:h4 "Normal, IL"]
      [:p "I was born here, most of my family is here, and I don't get out of here often. I do love this town though."]]))

(defpage "/partials/places/chicago" []
  (html
    [:div.info-window
      [:h4 "Chicago, IL"]
      [:p "I haven't spent too much time here, but I do go on occasion for conferences and professional gatherings. I'll probably work downtown some day."]]))

(defpage "/partials/restaurants" []
  (html
    [:h2 "Restaurants"]
    [:div.tabbable
      [:ul#restaurant_tabs.nav.nav-tabs
        [:li.active
          [:a {:href "#restaurant_tab_1" :data-toggle "tab"} "Restaurant List"]]
        [:li
          [:a {:href "#restaurant_tab_2" :data-gtoggle "tab"} "{{{true:'Edit',false:'Add'}[edit]}} Restaurant"]]]
      [:div.tab-content
        [:div#restaurant_tab_1.tab-pane.active
          [:table.table.table-bordered.table-striped.table-hover
            [:thead
              [:tr
                [:th 
                  [:a {:ng-click "column='restaurant_name';reverse=false" :ng-show "column!='restaurant_name'"} "Name"]
                  [:a {:ng-click "reverse=!reverse" :ng-show "column=='restaurant_name'"} "Name"]
                  [:a.reverse-icon {:ng-click "reverse=!reverse"}
                    [:i {:ng-show "column=='restaurant_name'"
                         :ng-class "{true:'icon-sort-up',false:'icon-sort-down'}[reverse]"}]]]
                [:th
                  [:a {:ng-click "column='address';reverse=false" :ng-show "column!='address'"} "Address"]
                  [:a {:ng-click "reverse=!reverse" :ng-show "column=='address'"} "Address"]
                  [:a.reverse-icon {:ng-click "reverse=!reverse"}
                    [:i {:ng-show "column=='address'"
                         :ng-class "{true:'icon-sort-up',false:'icon-sort-down'}[reverse]"}]]]
                [:th
                  [:a {:ng-click "column='city';reverse=false" :ng-show "column!='city'"} "City"]
                  [:a {:ng-click "reverse=!reverse" :ng-show "column=='city'"} "City"]
                  [:a.reverse-icon {:ng-click "reverse=!reverse"}
                    [:i {:ng-show "column=='city'"
                         :ng-class "{true:'icon-sort-up',false:'icon-sort-down'}[reverse]"}]]]
                [:th
                  [:a {:ng-click "column='state';reverse=false" :ng-show "column!='state'"} "St"]
                  [:a {:ng-click "reverse=!reverse" :ng-show "column=='state'"} "St"]
                  [:a.reverse-icon {:ng-click "reverse=!reverse"}
                    [:i {:ng-show "column=='state'"
                         :ng-class "{true:'icon-sort-up',false:'icon-sort-down'}[reverse]"}]]]
                [:th
                  [:a {:ng-click "column='zip';reverse=false" :ng-show "column!='zip'"} "Zip"]
                  [:a {:ng-click "reverse=!reverse" :ng-show "column=='zip'"} "Zip"]
                  [:a.reverse-icon {:ng-click "reverse=!reverse"}
                    [:i {:ng-show "column=='zip'"
                         :ng-class "{true:'icon-sort-up',false:'icon-sort-down'}[reverse]"}]]]
                [:th "Edit"]
                [:th "Delete"]]
            [:colgroup
              [:col.span2]
              [:col.span2]
              [:col.span2]
              [:col.span1]
              [:col.span1]
              [:col.span1]
              [:col.span1]]]
            [:tbody 
              {:ng-repeat 
                "restaurant in restaurantList | orderBy:column:reverse"}
              [:tr 
                [:td "{{restaurant.restaurant_name}}"]
                [:td "{{restaurant.address}}"]
                [:td "{{restaurant.city}}"]
                [:td "{{restaurant.state}}"]
                [:td "{{restaurant.zip}}"]
                [:td.action-icon
                  [:a 
                    {:ng-click 
                      (str "update(restaurant.restaurant_name,"
                           "restaurant.address,"
                           "restaurant.city,"
                           "restaurant.state,"
                           "restaurant.zip)")}
                    [:i.icon-edit]]]
                [:td.action-icon
                  [:a 
                    {:ng-click "$parent.confirm=$index"
                     :ng-show "$parent.confirm!=$index"}
                    [:i.icon-trash]]
                  [:a 
                    {:ng-click "$parent.confirm=-1"
                     :ng-show "$parent.confirm==$index"}
                    [:i.icon-ban-circle]]]]
              [:tr {:ng-show "$parent.confirm==$index"}
                [:td.confirm-delete {:colspan "7"}
                 [:a.btn.btn-danger 
                   {:ng-click "remove(restaurant.restaurant_name);$parent.confirm=-1"}
                   "Confirm Delete"]]]]]]
        [:div#restaurant_tab_2.tab-pane
          [:div#add_restaurant_placeholder]
          [:form#add_restaurant.form-horizontal {:name "add_restaurant" :ng-submit "save()"}
            [:fieldset
              [:div.control-group {:ng-class "{error: add_restaurant.restaurant_name.$invalid}"}
                [:label.control-label {:for "add_restaurant_name"} "Name: "]
                [:div.controls
                  [:input#add_restaurant_name.input-xlarge {:ng-model "restaurant_name" :type "text" :name "restaurant_name" :maxlength "35" :required ""}]
                  [:span.help-inline {:ng-show "add_restaurant.restaurant_name.$invalid"} "Name must be non-empty"]
                  [:p "Enter the restaurant name"]]]
              [:div.control-group {:ng-class "{error: add_restaurant.address.$invalid}"}
                [:label.control-label {:for "add_restaurant_address"} "Address: "]
                [:div.controls
                  [:input#add_restaurant_address.input-xlarge {:ng-model "address" :type "text" :name "address" :maxlength "40" :required ""}]
                  [:span.help-inline {:ng-show "add_restaurant.address.$invalid"} "Address must be non-empty"]
                  [:p "Enter the restaurant's street address"]]]
              [:div.control-group {:ng-class "{error: add_restaurant.city.$invalid}"}
                [:label.control-label {:for "add_restaurant_city"} "City: "]
                [:div.controls
                  [:input#add_restaurant_city.input-xlarge {:ng-model "city" :type "text" :name "city" :maxlength "30" :required ""}]
                  [:span.help-inline {:ng-show "add_restaurant.city.$invalid"} "City must be non-empty"]
                  [:p "Enter the restaurant's city"]]]
              [:div.control-group {:ng-class "{error: add_restaurant.state.$invalid}"}
                [:label.control-label {:for "add_restaurant_state"} "State: "]
                [:div.controls
                  [:select#add_restaurant_state {:ng-model "state" :name "state" :required ""}
                    [:option {:value ""} "-- Select a State --"] 
                    [:option {:value "AL"} "Alabama"]
                    [:option {:value "AK"} "Alaska"]
                    [:option {:value "AZ"} "Arizona"]   
                    [:option {:value "AR"} "Arkansas"]
                    [:option {:value "CA"} "California"]
                    [:option {:value "CO"} "Colorado"]
                    [:option {:value "CT"} "Connecticut"]
                    [:option {:value "DE"} "Delaware"]
                    [:option {:value "DC"} "District Of Columbia"]
                    [:option {:value "FL"} "Florida"]
                    [:option {:value "GA"} "Georgia"]
                    [:option {:value "HI"} "Hawaii"]
                    [:option {:value "ID"} "Idaho"]
                    [:option {:value "IL"} "Illinois"]
                    [:option {:value "IN"} "Indiana"]
                    [:option {:value "IA"} "Iowa"]
                    [:option {:value "KS"} "Kansas"]
                    [:option {:value "KY"} "Kentucky"]
                    [:option {:value "LA"} "Louisiana"]
                    [:option {:value "ME"} "Maine"]
                    [:option {:value "MD"} "Maryland"]
                    [:option {:value "MA"} "Massachusetts"]
                    [:option {:value "MI"} "Michigan"]
                    [:option {:value "MN"} "Minnesota"]
                    [:option {:value "MS"} "Mississippi"]
                    [:option {:value "MO"} "Missouri"]
                    [:option {:value "MT"} "Montana"]
                    [:option {:value "NE"} "Nebraska"]
                    [:option {:value "NV"} "Nevada"]
                    [:option {:value "NH"} "New Hampshire"]
                    [:option {:value "NJ"} "New Jersey"]
                    [:option {:value "NM"} "New Mexico"]
                    [:option {:value "NY"} "New York"]
                    [:option {:value "NC"} "North Carolina"]
                    [:option {:value "ND"} "North Dakota"]
                    [:option {:value "OH"} "Ohio"]
                    [:option {:value "OK"} "Oklahoma"]
                    [:option {:value "OR"} "Oregon"]
                    [:option {:value "PA"} "Pennsylvania"]
                    [:option {:value "RI"} "Rhode Island"]
                    [:option {:value "SC"} "South Carolina"]
                    [:option {:value "SD"} "South Dakota"]
                    [:option {:value "TN"} "Tennessee"]
                    [:option {:value "TX"} "Texas"]
                    [:option {:value "UT"} "Utah"]
                    [:option {:value "VT"} "Vermont"]
                    [:option {:value "VA"} "Virginia"]
                    [:option {:value "WA"} "Washington"]
                    [:option {:value "WV"} "West Virginia"]
                    [:option {:value "WI"} "Wisconsin"]
                    [:option {:value "WY"} "Wyoming"]]
                  [:span.help-inline {:ng-show "add_restaurant.state.$invalid"} "You must choose a state."]
                  [:p "Enter the restaurant's state"]]]
              [:div.control-group {:ng-class "{error: add_restaurant.zip.$invalid}"}
                [:label.control-label {:for "add_restaurant_zip"} "Zip: "]
                [:div.controls
                  [:input#add_restaurant_zip.input-xlarge {:ng-model "zip" :type "text" :name "zip" :maxlength "5" :required "" :ng-pattern "/^\\d{5}$/"}]
                  [:span.help-inline {:ng-show "add_restaurant.zip.$invalid"} "Zip must be non-empty and 5 digits"]
                  [:p "Enter the restaurant's zip"]]]
              [:div.form-actions
                [:button.btn.btn-primary {:type "submit" :ng-disabled "isClean() || add_restaurant.$invalid"} "Save"]
                [:button#add_restaurant_reset.btn {:ng-click "reset()"} "Cancel"]]]]]]]))

(defpage "/db/restaurants" []
  (response/json (db/get-restaurants)))

(defpage [:post "/db/restaurants"] {:keys [restaurant_name address city state zip]}
  (if-let [new-list (db/update-restaurant restaurant_name address city state zip)]
    (response/json new-list)
    (response/empty)))

(defpage [:delete "/db/restaurants"] {:keys [restaurant_name]}
  (if-let [new-list (db/rem-restaurant restaurant_name)]
    (response/json new-list)
    (response/empty)))

(defpage "/partials/examples" []
  (html
    [:div.row
      [:div.span9
        [:div#carousel.carousel.slide
          [:div.carousel-inner
            [:div.item
              [:img {:alt "PitchIt" :src (utils/add-context "/img/pitchit.png")}]
              [:div.carousel-caption
                [:h4 
                  [:a {:href "http://brian-jesse.com/pitchit"} "PitchIt"]]
                [:p "An application developed for Illinois State University. PitchIt is an application that helps a class of future entrepreneurs develop their business plan."]]]
            [:div.item
              [:img {:alt "GoalCharge" :src (utils/add-context "/img/goalcharge.png")}]
              [:div.carousel-caption
                [:h4 
                  [:a {:href "http://goalcharge.com"}"GoalCharge"]]
                [:p "GoalCharge was first concieved during a mobile application development competion. It has since been ported for use on desktop and mobile devices."]]]
            [:div.item
              [:img {:alt "Leah Walk" :src (utils/add-context "/img/leahwalk.png")}]
              [:div.carousel-caption
                [:h4 
                  [:a {:href "http://leahwalk.com"} "Leah Walk"]]
                [:p "The Leah Memorial Walk site is an example of simple elegant design"]]]]
          [:a.carousel-control.left {:href "#carousel" :data-slide "prev"} "&lsaquo;"]
          [:a.carousel-control.right {:href "#carousel" :data-slide "next"} "&rsaquo;"]
         ]]]
    [:div.row
      [:div.span4 
        [:h4 "Front End Framework Knowledge"]
        [:ul 
          [:li "Angular JS"]
          [:li "Twitter Bootstrap"]
          [:li "jQuery"]]]
      [:div.span4
        [:h4 "Back End Development Platforms"]
        [:ul 
          [:li "PHP - Codeigniter"]
          [:li "Java EE6 - Glassfish"]
          [:li "Clojure - Noir"]
          [:li "Python - Flask"]]]
     [:div.row
      [:div#bootstraplove.span8
        [:hr]
        [:h3 "Twitter Bootstrap"]
        [:p "Bootstrap gives you the ability to rapidly develop nice looking websites in a shorter amount of time. With the default styles from Bootstrap you have sensible margins and padding on useful elements, nice layout for standard ui items, and decent typography. Utilizing the grid system makes laying out your page simple. Bootstrap's responsive template is simple to use and radically changes the way mobile sites are constructed."]
        [:p "Bootstrap also enforces standard practices when creating forms and laying out your page. Bootstrap is compatible with older and modern browsers, so you aren't at a disadvantage when offering backwards compatibility."]]]]))

(defonce pic-order (atom [1 2 3 4]))

(defpage [:post "/ws/picorder"] {:keys [order]}
  (response/json 
    (reset! pic-order order)))

(defpage "/ws/picorder" []
  (response/json @pic-order))

(defpage "/partials/gallery" []
  (html
    [:div.tabbable
      [:ul#gallery_tabs.nav.nav-tabs
        [:li.active
          [:a {:href "#gallery_tab_1" :data-toggle "tab"} "Gallery"]]
        [:li
          [:a {:href "#gallery_tab_2" :data-gtoggle "tab"} "Reorder"]]]
      [:div.tab-content
        [:div#gallery_tab_1.tab-pane.active
          [:div.row {}
            [:div.span2 {:ng-repeat "image in pics" :fancy-box-directive ""}
              [:a {:rel "group" :href (utils/add-context "/img/gallery/{{image}}.jpg")}
                [:img.img-rounded {:alt "" :ng-src (utils/add-context "/img/gallery/{{image}}_sm.jpg")}]]]]]
        [:div#gallery_tab_2.tab-pane
          [:ul.row {:sortable-directive ""}
            [:li.span2 {:ng-repeat "image in pics" :id "{{image}}"}
              [:img.img-rounded {:ng-src (utils/add-context "/img/gallery/{{image}}_sm.jpg")}]]]
          [:div.row
           [:div.span3]
           [:div.span3
            [:a.btn {:ng-click "saveOrder()"} "Save Order"]]]]]]))
