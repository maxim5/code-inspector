package net.ontrack.client.support;

import net.ontrack.client.ManageUIClient;
import net.ontrack.core.model.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Collection;
import java.util.List;
import java.util.Locale;

import static java.lang.String.format;

public class DefaultManageUIClient extends AbstractClient implements ManageUIClient {

    public DefaultManageUIClient(String url) {
        super(url);
    }

    @Override
    public String getProjectURL(String project) {
        return getUrl(format("/gui/project/%s", project));
    }

    @Override
    public String getBranchURL(String project, String branch) {
        return getUrl(format("/gui/project/%s/branch/%s", project, branch));
    }

    @Override
    public String getPromotionLevelImageURL(String project, String branch, String name) {
        return getUrl(format("/ui/manage/project/%s/branch/%s/promotion_level/%s/image", project, branch, name));
    }

    @Override
    public String getValidationStampImageURL(String project, String branch, String name) {
        return getUrl(format("/ui/manage/project/%s/branch/%s/validation_stamp/%s/image", project, branch, name));
    }

    @Override
    public String getVersion() {
        return get(getDefaultLocale(), "/ui/manage/version", String.class);
    }

    @Override
    public List<ProjectSummary> getProjectList() {
        return list(getDefaultLocale(), "/ui/manage/project", ProjectSummary.class);
    }

    @Override
    public ProjectSummary createProject(ProjectCreationForm form) {
        return post(getDefaultLocale(), "/ui/manage/project", ProjectSummary.class, form);
    }

    @Override
    public ProjectSummary getProject(String name) {
        return get(getDefaultLocale(), format("/ui/manage/project/%s", name), ProjectSummary.class);
    }

    @Override
    public Ack deleteProject(String name) {
        return delete(getDefaultLocale(), format("/ui/manage/project/%s", name), Ack.class);
    }

    @Override
    public ProjectSummary updateProject(String name, ProjectUpdateForm form) {
        return put(getDefaultLocale(), format("/ui/manage/project/%s", name), ProjectSummary.class, form);
    }

    @Override
    public List<BranchSummary> getBranchList(String project) {
        return list(getDefaultLocale(), format("/ui/manage/project/%s/branch", project), BranchSummary.class);
    }

    @Override
    public BranchSummary getBranch(String project, String name) {
        return get(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s", project, name), BranchSummary.class);
    }

    @Override
    public BranchCloneInfo getBranchCloneInfo(Locale locale, String project, String name) {
        return get(locale, format("/ui/manage/project/%s/branch/%s/clone", project, name), BranchCloneInfo.class);
    }

    @Override
    public DecoratedBranch getDecoratedBranch(Locale locale, String project, String name) {
        return get(locale, format("/ui/manage/project/%s/branch/%s/decorated", project, name), DecoratedBranch.class);
    }

    @Override
    public BranchFilterData getBranchFilterData(Locale locale, String project, String branch) {
        return get(locale, format("/ui/manage/project/%s/branch/%s/filter", project, branch), BranchFilterData.class);
    }

    @Override
    public BuildCleanup getBuildCleanup(String project, String branch) {
        return get(
                getDefaultLocale(),
                format(
                        "/ui/manage/project/%s/branch/%s/cleanup",
                        project,
                        branch
                ),
                BuildCleanup.class
        );
    }

    @Override
    public Ack setBuildCleanup(String project, String branch, BuildCleanupForm form) {
        return put(
                getDefaultLocale(),
                format(
                        "/ui/manage/project/%s/branch/%s/cleanup",
                        project,
                        branch
                ),
                Ack.class,
                form
        );
    }

    @Override
    public BranchSummary createBranch(String project, BranchCreationForm form) {
        return post(getDefaultLocale(), format("/ui/manage/project/%s/branch", project), BranchSummary.class, form);
    }

    @Override
    public BranchSummary updateBranch(String project, String name, BranchUpdateForm form) {
        return put(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s", project, name), BranchSummary.class, form);
    }

    @Override
    public BranchSummary cloneBranch(String project, String name, BranchCloneForm form) {
        return post(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/clone", project, name), BranchSummary.class, form);
    }

    @Override
    public Ack deleteBranch(String project, String name) {
        return delete(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s", project, name), Ack.class);
    }

    @Override
    public List<ValidationStampSummary> getValidationStampList(String project, String branch) {
        return list(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/validation_stamp", project, branch), ValidationStampSummary.class);
    }

    @Override
    public ValidationStampSummary getValidationStamp(String project, String branch, String name) {
        return get(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/validation_stamp/%s", project, branch, name), ValidationStampSummary.class);
    }

    @Override
    public DecoratedValidationStamp getDecoratedValidationStamp(Locale locale, String project, String branch, String validationStamp) {
        return get(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/validation_stamp/%s/decorated", project, branch, validationStamp), DecoratedValidationStamp.class);
    }

    @Override
    public ValidationStampSummary createValidationStamp(String project, String branch, ValidationStampCreationForm form) {
        return post(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/validation_stamp", project, branch), ValidationStampSummary.class, form);
    }

    @Override
    public ValidationStampSummary updateValidationStamp(String project, String branch, String validationStamp, ValidationStampUpdateForm form) {
        return put(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/validation_stamp/%s", project, branch, validationStamp), ValidationStampSummary.class, form);
    }

    @Override
    public Ack deleteValidationStamp(String project, String branch, String validationStamp) {
        return delete(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/validation_stamp/%s", project, branch, validationStamp), Ack.class);
    }

    @Override
    public Ack addValidationStampComment(String project, String branch, String validationStamp, ValidationStampCommentForm form) {
        return post(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/validation_stamp/%s/comment", project, branch, validationStamp), Ack.class, form);
    }

    @Override
    public Collection<Comment> getValidationStampComments(Locale locale, String project, String branch, String validationStamp, int offset, int count) {
        return list(locale, format("/ui/manage/project/%s/branch/%s/validation_stamp/%s/comment?offset=%d&count=%d", project, branch, validationStamp, offset, count), Comment.class);
    }

    @Override
    public Ack setImageValidationStamp(String project, String branch, String name, MultipartFile image) {
        return upload(
                getDefaultLocale(),
                format("/ui/manage/project/%s/branch/%s/validation_stamp/%s/image", project, branch, name),
                "image",
                image,
                Ack.class);
    }

    @Override
    public byte[] imageValidationStamp(String project, String branch, String name) {
        return getBytes(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/validation_stamp/%s/image", project, branch, name));
    }

    @Override
    public Ack linkValidationStampToPromotionLevel(String project, String branch, String validationStamp, String promotionLevel) {
        return get(
                getDefaultLocale(),
                format(
                        "/ui/manage/project/%s/branch/%s/validation_stamp/%s/link/%s",
                        project,
                        branch,
                        validationStamp,
                        promotionLevel
                ),
                Ack.class);
    }

    @Override
    public Ack unlinkValidationStampToPromotionLevel(String project, String branch, String validationStamp) {
        return get(
                getDefaultLocale(),
                format(
                        "/ui/manage/project/%s/branch/%s/validation_stamp/%s/unlink",
                        project,
                        branch,
                        validationStamp
                ),
                Ack.class);
    }

    @Override
    public Ack upValidationStamp(String project, String branch, String validationStamp) {
        return put(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/validation_stamp/%s/up", project, branch, validationStamp), Ack.class, null);
    }

    @Override
    public Ack downValidationStamp(String project, String branch, String validationStamp) {
        return put(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/validation_stamp/%s/down", project, branch, validationStamp), Ack.class, null);
    }

    @Override
    public Ack setValidationStampOwner(String project, String branch, String validationStamp, int ownerId) {
        return put(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/validation_stamp/%s/owner/%d", project, branch, validationStamp, ownerId), Ack.class, null);
    }

    @Override
    public Ack unsetValidationStampOwner(String project, String branch, String validationStamp) {
        return delete(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/validation_stamp/%s/owner", project, branch, validationStamp), Ack.class);
    }

    @Override
    public PromotionLevelSummary getPromotionLevel(String project, String branch, String name) {
        return get(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/promotion_level/%s", project, branch, name), PromotionLevelSummary.class);
    }

    @Override
    public PromotionLevelSummary createPromotionLevel(String project, String branch, PromotionLevelCreationForm form) {
        return post(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/promotion_level", project, branch), PromotionLevelSummary.class, form);
    }

    @Override
    public PromotionLevelSummary updatePromotionLevel(String project, String branch, String promotionLevel, PromotionLevelUpdateForm form) {
        return put(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/promotion_level/%s", project, branch, promotionLevel), PromotionLevelSummary.class, form);
    }

    @Override
    public Ack deletePromotionLevel(String project, String branch, String name) {
        return delete(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/promotion_level/%s", project, branch, name), Ack.class);
    }

    @Override
    public Ack setImagePromotionLevel(String project, String branch, String name, MultipartFile image) {
        return upload(
                getDefaultLocale(),
                format("/ui/manage/project/%s/branch/%s/promotion_level/%s/image", project, branch, name),
                "image",
                image,
                Ack.class);
    }

    @Override
    public byte[] imagePromotionLevel(String project, String branch, String name) {
        return getBytes(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/promotion_level/%s/image", project, branch, name));
    }

    @Override
    public List<PromotionLevelSummary> getPromotionLevelList(String project, String branch) {
        return list(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/promotion_level", project, branch), PromotionLevelSummary.class);
    }

    @Override
    public Ack upPromotionLevel(String project, String branch, String promotionLevel) {
        return get(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/promotion_level/%s/up", project, branch, promotionLevel), Ack.class);
    }

    @Override
    public Ack downPromotionLevel(String project, String branch, String promotionLevel) {
        return get(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/promotion_level/%s/down", project, branch, promotionLevel), Ack.class);
    }

    @Override
    public PromotionLevelManagementData getPromotionLevelManagementData(String project, String branch) {
        return get(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/promotion_level_manage", project, branch), PromotionLevelManagementData.class);
    }

    @Override
    public PromotionLevelAndStamps getPromotionLevelValidationStamps(String project, String branch, String promotionLevel) {
        return get(
                getDefaultLocale(),
                format("/ui/manage/project/%s/branch/%s/promotion_level/%s", project, branch, promotionLevel),
                PromotionLevelAndStamps.class);
    }

    @Override
    public Flag setPromotionLevelAutoPromote(String project, String branch, String promotionLevel) {
        return put(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/promotion_level/%s/autopromote/set", project, branch, promotionLevel), Flag.class, null);
    }

    @Override
    public Flag unsetPromotionLevelAutoPromote(String project, String branch, String promotionLevel) {
        return put(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/promotion_level/%s/autopromote/unset", project, branch, promotionLevel), Flag.class, null);
    }

    @Override
    public BranchBuilds getBuilds(Locale locale, String project, String branch, BuildFilter filter) {
        return post(locale, format("/ui/manage/project/%s/branch/%s/query", project, branch), BranchBuilds.class, filter);
    }

    @Override
    public Ack deleteBuild(String project, String branch, String build) {
        return delete(
                getDefaultLocale(),
                format(
                        "/ui/manage/project/%s/branch/%s/build/%s",
                        project,
                        branch,
                        build
                ),
                Ack.class
        );
    }

    @Override
    public BuildSummary getBuild(String project, String branch, String name) {
        return get(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/build/%s", project, branch, name), BuildSummary.class);
    }

    @Override
    public BuildSummary updateBuild(String project, String branch, String build, BranchUpdateForm form) {
        return put(
                getDefaultLocale(),
                format("/ui/manage/project/%s/branch/%s/build/%s", project, branch, build),
                BuildSummary.class,
                form
        );
    }

    @Override
    public BuildSummary getLastBuild(String project, String branch) {
        return get(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/build/last", project, branch), BuildSummary.class);
    }

    @Override
    public BuildSummary getLastBuildWithValidationStamp(Locale locale, String project, String branch, String validationStamp) {
        return get(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/build/withValidationStamp/%s", project, branch, validationStamp), BuildSummary.class);
    }

    @Override
    public BuildSummary getLastBuildWithPromotionLevel(Locale locale, String project, String branch, String promotionLevel) {
        return get(getDefaultLocale(), format("/ui/manage/project/%s/branch/%s/build/withPromotionLevel/%s", project, branch, promotionLevel), BuildSummary.class);
    }

    @Override
    public List<BuildValidationStamp> getBuildValidationStamps(Locale locale, String project, String branch, String name) {
        return list(locale, format("/ui/manage/project/%s/branch/%s/build/%s/validationStamps", project, branch, name), BuildValidationStamp.class);
    }

    @Override
    public List<BuildPromotionLevel> getBuildPromotionLevels(Locale locale, String project, String branch, String name) {
        return list(locale, format("/ui/manage/project/%s/branch/%s/build/%s/promotionLevels", project, branch, name), BuildPromotionLevel.class);
    }

    @Override
    public ValidationRunSummary getValidationRun(String project, String branch, String build, String validationStamp, int run) {
        return get(
                getDefaultLocale(),
                format(
                        "/ui/manage/project/%s/branch/%s/build/%s/validation_stamp/%s/validation_run/%d",
                        project,
                        branch,
                        build,
                        validationStamp,
                        run
                ),
                ValidationRunSummary.class
        );
    }

    @Override
    public List<ValidationRunEvent> getValidationRunHistory(Locale locale, int validationRunId, int offset, int count) {
        return list(locale, format("/ui/manage/validation_run/%d/history?offset=%d&count=%d", validationRunId, offset, count), ValidationRunEvent.class);
    }

    @Override
    public List<ValidationRunEvent> getValidationRunsForValidationStamp(Locale locale, String project, String branch, String validationStamp, int offset, int count) {
        return list(
                locale,
                format(
                        "/ui/manage/project/%s/branch/%s/validationStamp/%s/validation_run?offset=%d&count=%d",
                        project,
                        branch,
                        validationStamp,
                        offset,
                        count),
                ValidationRunEvent.class);
    }

    @Override
    public Ack addValidationRunComment(int runId, ValidationRunCommentCreationForm form) {
        return post(
                getDefaultLocale(),
                format(
                        "/ui/manage/validation_run/%d/comment",
                        runId
                ),
                Ack.class,
                form
        );
    }

    @Override
    public Ack deleteValidationRun(String project, String branch, String build, String validationStamp, int runOrder) {
        return delete(
                getDefaultLocale(),
                format(
                        "/ui/manage/project/%s/branch/%s/build/%s/validation_stamp/%s/validation_run/%d",
                        project,
                        branch,
                        build,
                        validationStamp,
                        runOrder
                ),
                Ack.class
        );
    }

    @Override
    public List<Promotion> getPromotions(Locale locale, String project, String branch, String promotionLevel, int offset, int count) {
        return list(
                locale,
                format(
                        "/ui/manage/project/%s/branch/%s/promotion_level/%s/promotions?offset=%d&count=%d",
                        project,
                        branch,
                        promotionLevel,
                        offset,
                        count
                ),
                Promotion.class
        );
    }

    @Override
    public Ack removePromotedRun(String project, String branch, String build, String promotionLevel) {
        return delete(
                getDefaultLocale(),
                format(
                        "/ui/manage/project/%s/branch/%s/build/%s/promotion_level/%s",
                        project,
                        branch,
                        build,
                        promotionLevel
                ),
                Ack.class
        );
    }

}
