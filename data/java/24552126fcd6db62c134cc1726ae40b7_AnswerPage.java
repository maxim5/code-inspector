/*
 * OpenVRef, A virtual reference service.
 *
 * Copyright (c) 2008, The Alberta Library
 * Fletcher Nichol and Natasha Nunn, wrote this for The Alberta Library
 * and the Alberta Public Library Electronic Network
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


package ca.talonline.openvref.web.page.staff;

import ca.talonline.openvref.domain.AuditEvent;
import ca.talonline.openvref.domain.PersonalComment;
import ca.talonline.openvref.domain.Request;
import ca.talonline.openvref.service.CollaborationService;
import ca.talonline.openvref.service.RequestService;
import ca.talonline.openvref.service.StaffService;
import ca.talonline.openvref.util.Growler;
import ca.talonline.openvref.util.HtmlRenderer;
import ca.talonline.openvref.util.LinkCreator;
import ca.talonline.openvref.web.basic.DateLabel;
import ca.talonline.openvref.web.basic.ResourceLabel;
import ca.talonline.openvref.web.behavior.InputFocusBehavior;
import ca.talonline.openvref.web.button.action.CollaborateButton;
import ca.talonline.openvref.web.markup.html.form.TimeSpentChoiceRenderer;
import ca.talonline.openvref.web.model.DetachableRequestModel;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;
import org.apache.wicket.Component;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.PageParameters;
import org.apache.wicket.ajax.AbstractAjaxTimerBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.CheckBoxMultipleChoice;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.StringResourceModel;
import org.apache.wicket.protocol.http.RequestUtils;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.time.Duration;
import org.wicketstuff.jquery.JQueryBehavior;


/**
 *
 * @author Fletcher Nichol &lt;fletcher@silversky.ca&gt;
 */
public class AnswerPage extends StaffBasePage {

    private static final long serialVersionUID = 1L;

    @SpringBean(name = "requestService")
    private RequestService requestService;

    @SpringBean(name = "staffService")
    private StaffService staffService;

    @SpringBean(name = "collaborationService")
    private CollaborationService collaborationService;

    @SpringBean(name = "htmlRenderer")
    private HtmlRenderer htmlRenderer;

    public AnswerPage(final UUID requestUUID) {
        super();
        setModel(new DetachableRequestModel(requestUUID, requestService));
        createComponents();
    }

    public AnswerPage(PageParameters parameters) {
        super(parameters);
        setModel(new DetachableRequestModel(UUID.fromString(parameters.getString(
                "uuid")), requestService));
        createComponents();
    }

    @Override
    protected void setTitleAsStringResource(String key) {
        Request request = (Request) getModelObject();
        setTitle(new StringResourceModel(key, this, null, new Object[]{
                    request.getLocalId()
                }));
    }

    @Override
    protected String getTitleResourceString() {
        return "title.page.answer";
    }

    private TextArea buildAnswer() {
        TextArea answer = newRequiredTextArea("answer");
        answer.add(new AjaxFormComponentUpdatingBehavior("onkeyup") {

            private static final long serialVersionUID = 1L;

            @Override
            protected void onUpdate(AjaxRequestTarget target) {
            }
        }.setThrottleDelay(Duration.seconds(5)));
        return answer;
    }

    private CollaborateButton buildCollaborateButton(final Request request) {
        return new CollaborateButton("collaborateLink", request.getUUID());
    }

    private CheckBoxMultipleChoice buildSubjectList() {
        CheckBoxMultipleChoice subjects =
                new CheckBoxMultipleChoice("subjectList",
                buildSubjectListChoices()).setPrefix("<li>").setSuffix("</li>");
        subjects.add(new AjaxFormChoiceComponentUpdatingBehavior() {

            private static final long serialVersionUID = 1L;

            @Override
            protected void onUpdate(AjaxRequestTarget target) {
            }
        });
        return subjects;
    }

    private RadioChoice buildTimeSpentMinutes() {
        RadioChoice timeSpent =
                new RadioChoice("timeSpentMinutes", buildTimeSpentChoices(),
                new TimeSpentChoiceRenderer()).setSuffix("</li>").
                setPrefix("<li>");
        timeSpent.add(new AjaxFormChoiceComponentUpdatingBehavior() {

            private static final long serialVersionUID = 1L;

            @Override
            protected void onUpdate(AjaxRequestTarget target) {
            }
        });
        return timeSpent;
    }

    private Component buildPersonalComments(Request request) {
        if (request.getPersonalComments() != null &&
                request.getPersonalComments().size() > 0) {
            return new PersonalCommentFragment("comments", "commentsFrag",
                    this, request).setRenderBodyOnly(false);
        } else {
            return new NoPersonalCommentFragment("comments", "noCommentsFrag",
                    this).setRenderBodyOnly(false);
        }
    }

    @SuppressWarnings("unchecked")
    private void createComponents() {
        final Request request = (Request) getModelObject();
        final Long localRequestId = request.getLocalId();

        add(new ResourceLabel("bodyTitle", "title.body.page.answer",
                new Object[]{localRequestId}));
        final FeedbackPanel feedback = newFeedbackPanel("feedback");
        add(feedback);

        Form form = new Form("answerForm", new CompoundPropertyModel(request));
        form.add(buildTimeSpentMinutes());
        form.addOrReplace(buildPersonalComments(request));
        form.add(buildCollaborateButton(request));
        form.add(new Label("clientEmail", request.getClientEmail()).
                setRenderBodyOnly(
                true));
        form.add(new DateLabel("postedDate", request.getPostedDate()).
                setRenderBodyOnly(true));
        form.add(new Label("question",
                htmlRenderer.render(request.getQuestion())).
                setEscapeModelStrings(false));
        form.add(buildAnswer());
        form.add(new SaveAsDraftButton());
        form.add(new SendAndCloseButton());
        form.add(buildSubjectList());
        form.add(new AbstractAjaxTimerBehavior(Duration.minutes(10)) {

            private static final long serialVersionUID = 1L;

            @Override
            protected void onTimer(AjaxRequestTarget target) {
                Request formState = (Request) target.getPage().
                        get("answerForm").getModelObject();
                Request dbState = requestService.getRequest(formState.getUUID());

                // if the answer in the form is different than the persited one,
                // save a draft
                if (dbState.getStatus().equals(Request.Status.DRAFT) &&
                        (!formState.getAnswer().equals(dbState.getAnswer()) ||
                        formState.getTimeSpentMinutes() !=
                        dbState.getTimeSpentMinutes() ||
                        !formState.getSubjectList().equals(
                        dbState.getSubjectList()))) {
                    doAutomaticSaveDraft(formState);
                }
                dbState = null;

                // prevent wicket changing focus
                target.focusComponent(null);

                for (String messageJs : AnswerPage.this.getMessagePager().
                        getMessagesAsJavascript()) {
                    target.appendJavascript(messageJs);
                }
            }
        });
        add(form);

        add(new ListView("auditLog", new ArrayList(request.getAuditLog())) {

            private static final long serialVersionUID = 1L;

            @Override
            protected void populateItem(ListItem item) {
                final AuditEvent event = (AuditEvent) item.getModelObject();
                item.add(new DateLabel("eventTimestamp", event.getTimestamp()));
                item.add(new Label("eventMessage", event.getMessage()));
            }
        });

        add(new Label("collabQuestionNum1", localRequestId.toString()));
        add(new Label("collabUser", getSessionUser().getUsername()));
        add(new Label("collabQuestionNum2", localRequestId.toString()));
        add(new Label("collabQuestion", request.getQuestion()));

        add(new CollaborateForm("collaborateForm", new Model(request)));

        buildPanelBehavior();
    }

    private List<String> buildSubjectListChoices() {
        List<String> subjectList = new LinkedList<String>();
        subjectList.add("Arts & Humanities/Architecture");
        subjectList.add("Arts & Humanities/Area Studies");
        subjectList.add("Arts & Humanities/Art & Collectibles");
        subjectList.add("Arts & Humanities/Design");
        subjectList.add("Arts & Humanities/History");
        subjectList.add("Arts & Humanities/Languages & Linguistics");
        subjectList.add("Arts & Humanities/Museums & Galleries");
        subjectList.add("Arts & Humanities/Occult & Paranormal");
        subjectList.add("Arts & Humanities/Performing Arts");
        subjectList.add("Arts & Humanities/Philosophy");
        subjectList.add("Arts & Humanities/Religion");
        subjectList.add("Arts & Humanities/Visual Arts");
        subjectList.add("Business & Employment/Banking & Investment");
        subjectList.add("Business & Employment/Business");
        subjectList.add("Business & Employment/Economics");
        subjectList.add("Business & Employment/Employment");
        subjectList.add("Business & Employment/Finance & Credit");
        subjectList.add("Business & Employment/Human Resources");
        subjectList.add("Business & Employment/Industry");
        subjectList.add("Business & Employment/Marketing");
        subjectList.add("Business & Employment/Small Business");
        subjectList.add("Current events/Alberta");
        subjectList.add("Current events/Canada");
        subjectList.add("Current events/International");
        subjectList.add("Current events/Local");
        subjectList.add("Education/Adult & Special Education");
        subjectList.add("Education/Educational Institutions");
        subjectList.add("Education/Higher Education");
        subjectList.add("Education/Instructional Technology & Tools");
        subjectList.add("Education/Primary & Secondary Education");
        subjectList.add("Education/Teaching & Pedagogy");
        subjectList.add("Engineering/Chemical");
        subjectList.add("Engineering/Civil");
        subjectList.add("Engineering/Construction");
        subjectList.add("Engineering/Electrical");
        subjectList.add("Engineering/Manufacturing");
        subjectList.add("Engineering/Mechanical & Industrial");
        subjectList.add("Engineering/Military");
        subjectList.add("Engineering/Transportation");
        subjectList.add("Entertainment & Recreation/Consumer Information");
        subjectList.add("Entertainment & Recreation/Entertainment & Leisure");
        subjectList.add("Entertainment & Recreation/Food & Cooking");
        subjectList.add("Entertainment & Recreation/Hobbies & Crafts");
        subjectList.add("Entertainment & Recreation/Home & Garden");
        subjectList.add("Entertainment & Recreation/Movies & TV");
        subjectList.add("Entertainment & Recreation/Music");
        subjectList.add("Entertainment & Recreation/Nature Activities");
        subjectList.add("Entertainment & Recreation/Pets");
        subjectList.add("Entertainment & Recreation/Sports");
        subjectList.add("Entertainment & Recreation/Trivia");
        subjectList.add("Health & Medicine/Disabilities");
        subjectList.add("Health & Medicine/Diseases & Conditions");
        subjectList.add("Health & Medicine/General health");
        subjectList.add("Health & Medicine/Human Physiology");
        subjectList.add("Health & Medicine/Medicine & Medical Services");
        subjectList.add("Health & Medicine/Pharmacology");
        subjectList.add("Health & Medicine/Public Health & Disease Prevention");
        subjectList.add("Health & Medicine/Sexuality & Reproduction");
        subjectList.add("Health & Medicine/Treatment");
        subjectList.add("Homework");
        subjectList.add("Information Technology/Computer Hardware");
        subjectList.add("Information Technology/Computer Industry");
        subjectList.add("Information Technology/Computer Science & Technology");
        subjectList.add("Information Technology/Gadgets");
        subjectList.add("Information Technology/Internet & Networking");
        subjectList.add("Information Technology/Software & Operating Systems");
        subjectList.add("Information Technology/Telecommunications");
        subjectList.add("Information Technology/Troubleshooting");
        subjectList.add("Literature & Libraries/Libraries & Information Studies");
        subjectList.add("Literature & Libraries/Literature & Biography");
        subjectList.add("Literature & Libraries/Publishing");
        subjectList.add("Literature & Libraries/Writing");
        subjectList.add("Local only/AAQ");
        subjectList.add("Local only/Library cards & accounts");
        subjectList.add("Local only/Library catalogue");
        subjectList.add("Local only/Library databases");
        subjectList.add("Local only/Library Other");
        subjectList.add("Local only/Library Policy");
        subjectList.add("Local only/Library programming");
        subjectList.add("Local only/Product purchase");
        subjectList.add("Other/Duplicate question");
        subjectList.add("Other/No Subject");
        subjectList.add("Other/Social comments");
        subjectList.add("Miscellaneous");
        subjectList.add("People & Places/Africa");
        subjectList.add("People & Places/Arctic Regions");
        subjectList.add("People & Places/Asia");
        subjectList.add("People & Places/Australia & Pacific");
        subjectList.add("People & Places/Central America & Caribbean");
        subjectList.add("People & Places/Ethnicity");
        subjectList.add("People & Places/Europe");
        subjectList.add("People & Places/First Nations");
        subjectList.add("People & Places/North America");
        subjectList.add("People & Places/South America");
        subjectList.add("People & Places/Travel & Regional Info");
        subjectList.add("Politics & Law/Government");
        subjectList.add("Politics & Law/Intellectual Property");
        subjectList.add("Politics & Law/Law & Regulation");
        subjectList.add("Politics & Law/Military");
        subjectList.add("Politics & Law/Politics & Elections");
        subjectList.add("Ready reference");
        subjectList.add("Science & Math/Agricultural & Animal Sciences");
        subjectList.add("Science & Math/Astronomy");
        subjectList.add("Science & Math/Biology");
        subjectList.add("Science & Math/Botany");
        subjectList.add("Science & Math/Chemistry");
        subjectList.add("Science & Math/Earth Sciences");
        subjectList.add("Science & Math/Ecology");
        subjectList.add("Science & Math/General Science");
        subjectList.add("Science & Math/History of Science & Invention");
        subjectList.add("Science & Math/Math & Statistics");
        subjectList.add("Science & Math/Physics");
        subjectList.add("Society & Social Science/Anthropology");
        subjectList.add("Society & Social Science/Archaeology");
        subjectList.add("Society & Social Science/Communications & Media");
        subjectList.add("Society & Social Science/Environment");
        subjectList.add("Society & Social Science/Parenting & Families");
        subjectList.add("Society & Social Science/Political Science");
        subjectList.add("Society & Social Science/Psychology");
        subjectList.add("Society & Social Science/Sexuality");
        subjectList.add("Society & Social Science/Social Activism");
        subjectList.add("Society & Social Science/Social Issues");
        subjectList.add("Society & Social Science/Sociology");
        return subjectList;
    }

    private List<Integer> buildTimeSpentChoices() {
        StringResourceModel timeSpentChoicesModel = new StringResourceModel(
                "values.timespent", AnswerPage.this, null);
        List<String> stringList = Arrays.asList(timeSpentChoicesModel.getString().
                split(" "));
        List<Integer> integerList = new LinkedList<Integer>();
        for (String string : stringList) {
            integerList.add(Integer.parseInt(string));
        }
        return integerList;
    }

    private TextArea newRequiredTextArea(String id) {
        TextArea textArea = new TextArea(id);
        textArea.setRequired(true);
        textArea.add(new InputFocusBehavior());
        textArea.setOutputMarkupId(true);
        return textArea;
    }

    protected void buildPanelBehavior() {
        add(new JQueryBehavior() {

            private static final long serialVersionUID = 1L;

            @Override
            protected CharSequence getOnReadyScript() {
                return "$('.extraExpanded').hide();" +
                        " $('.extraTitle').click(function() {" +
                        " $(this).next(\"div\").toggle(100);" +
                        " $(this).toggleClass(\"active\");" + " return false;" +
                        " });";
            }
        });
    }

    protected void doSaveAsDraft(Request request) {
        requestService.draftRequest(request, getSessionUser().getUUID());

        StringResourceModel auditMsgModel = new StringResourceModel(
                "audit.event.saveasdraft", this, null,
                new Object[]{
                    LinkCreator.createRequestLinkUrn(request).
                    render(),
                    LinkCreator.createStaffLinkUrn(getSessionUser()).
                    render()
                });
        requestService.auditRequest(request, auditMsgModel.getString());

        Growler.growl(this, "growl.event.saveasdraft",
                new Object[]{
                    request.getLocalId()
                });

    }

    protected void doAutomaticSaveDraft(Request request) {
        requestService.draftRequest(request, getSessionUser().getUUID());

        StringResourceModel auditMsgModel = new StringResourceModel(
                "audit.event.autosavedraft", this, null,
                new Object[]{
                    LinkCreator.createRequestLinkUrn(request).
                    render(),
                    LinkCreator.createStaffLinkUrn(getSessionUser()).
                    render()
                });
        requestService.auditRequest(request, auditMsgModel.getString());

        Growler.growl(this, "growl.event.autosavedraft",
                new Object[]{
                    request.getLocalId()
                });
    }


    private final class SendAndCloseButton extends Button {

        private static final long serialVersionUID = 1L;

        public SendAndCloseButton() {
            super("saveAndClose", new StringResourceModel("button.saveandclose",
                    null));
        }

        @Override
        public void onSubmit() {
            Request request = (Request) getForm().getModelObject();

            boolean isError = false;
            if (request.getTimeSpentMinutes() == 0) {
                error("You must choose 'Time Spent'.");
                isError = true;
            }
            if (request.getSubjectList() == null ||
                    request.getSubjectList().size() == 0) {
                error("You must assign at least one subject from the 'Subject List'.");
                isError = true;
            }
            if (isError) {
                doSaveAsDraft(request);
                return;
            }

            requestService.closeRequest(request, getSessionUser().getUUID());

            StringResourceModel auditMsgModel = new StringResourceModel(
                    "audit.event.saveandclose", this, null,
                    new Object[]{
                        LinkCreator.createRequestLinkUrn(request).
                        render(),
                        LinkCreator.createStaffLinkUrn(getSessionUser()).
                        render()
                    });
            StringResourceModel msgModel = new StringResourceModel(
                    "growl.event.saveandclose", this, null,
                    new Object[]{
                        request.getLocalId()
                    });

            requestService.auditRequest(request, auditMsgModel.getString());
            getSession().info(msgModel.getString());

            setResponsePage(MyClaimedQuestionsPage.class);
        }
    }


    private final class SaveAsDraftButton extends Button {

        private static final long serialVersionUID = 1L;

        public SaveAsDraftButton() {
            super("saveAsDraft", new StringResourceModel("button.saveasdraft",
                    null));
        }

        @Override
        public void onSubmit() {
            Request request = (Request) getForm().getModelObject();
            doSaveAsDraft(request);
        }
    }


    private class CollaborateForm extends Form {

        private static final long serialVersionUID = 1L;

        private TextArea textArea;

        public CollaborateForm(String id, IModel model) {
            super(id, model);
            textArea = new TextArea("collaborationMessage", new Model(""));
            textArea.setRequired(true);
            add(textArea);
        }

        @Override
        protected void onSubmit() {
            Request request = (Request) getModelObject();

            PageParameters viewPars = new PageParameters();
            viewPars.add("uuid", request.getUUID().toString());
            String relUrl = getRequestCycle().urlFor(ViewPage.class, viewPars).
                    toString();
            String reqPath = getWebRequestCycle().getWebRequest().
                    getHttpServletRequest().getRequestURL().toString();

            collaborationService.sendCollaboration(getSessionUser(),
                    request, textArea.getModelObjectAsString(),
                    RequestUtils.toAbsolutePath(reqPath, relUrl));

            StringResourceModel auditMsgModel =
                    new StringResourceModel(
                    "audit.event.collaborate", this, null,
                    new Object[]{
                        LinkCreator.createRequestLinkUrn(request).
                        render(),
                        LinkCreator.createStaffLinkUrn(getSessionUser()).
                        render()
                    });
            requestService.auditRequest(request, auditMsgModel.getString());

            Growler.growl(this, "growl.event.collaborate",
                    new Object[]{request.getLocalId()});
        }
    }


    private class PersonalCommentFragment extends Fragment {

        private static final long serialVersionUID = 1L;

        private Request request;

        public PersonalCommentFragment(String id, String markupId,
                                       MarkupContainer markupProvider,
                                       Request request) {
            super(id, markupId, markupProvider);
            this.request = request;
            add(new ListView("commentList", new ArrayList<PersonalComment>(
                    request.getPersonalComments())) {

                private static final long serialVersionUID = 1L;

                @Override
                protected void populateItem(ListItem item) {
                    PersonalComment comment =
                            (PersonalComment) item.getModelObject();
                    item.add(new ResourceLabel("info", "page.view.comment.info",
                            new Object[]{
                                staffService.getStaff(
                                comment.getSenderStaffUUID()).
                                getUsername(),
                                comment.getPostedDate()
                            }));
                    item.add(new Label("message", htmlRenderer.render(comment.
                            getComment())).setRenderBodyOnly(false).
                            setEscapeModelStrings(false));
                }
            });
        }
    }


    private class NoPersonalCommentFragment extends Fragment {

        private static final long serialVersionUID = 1L;

        public NoPersonalCommentFragment(String id, String markupId,
                                         MarkupContainer markupProvider) {
            super(id, markupId, markupProvider);
            add(new ResourceLabel("noComments", "page.view.comment.none"));
        }
    }
}
