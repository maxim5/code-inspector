package com.atlassian.plugins.codegen;

import java.io.File;
import java.io.FileInputStream;
import java.util.Properties;

import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static com.atlassian.plugins.codegen.ClassId.fullyQualified;
import static com.atlassian.plugins.codegen.I18nString.i18nString;
import static com.atlassian.plugins.codegen.PluginProjectChangeset.changeset;
import static com.atlassian.plugins.codegen.SourceFile.sourceFile;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class ProjectFilesRewriterTest
{
    protected static final ClassId CLASS = fullyQualified("com.atlassian.test.MyClass");
    protected static final String CONTENT = "this is some amazing content";
    protected static final String I18N_FILE_NAME = ProjectHelper.GROUP_ID + File.separator + ProjectHelper.ARTIFACT_ID + ".properties";

    protected ProjectHelper helper;
    protected ProjectFilesRewriter rewriter;
    protected File i18nFile;

    @Before
    public void setup() throws Exception
    {
        helper = new ProjectHelper();        
        usePluginXml("empty-plugin.xml");
    }
    
    private void usePluginXml(String path) throws Exception
    {
        helper.usePluginXml(path);
        rewriter = new ProjectFilesRewriter(helper.location);
        i18nFile = new File(helper.location.getResourcesDir(), I18N_FILE_NAME);
    }

    @After
    public void deleteTempDir() throws Exception
    {
        helper.destroy();
    }
    
    @Test
    public void sourceFileIsCreated() throws Exception
    {
        PluginProjectChangeset changes = new PluginProjectChangeset()
            .with(sourceFile(CLASS, SourceFile.SourceGroup.MAIN, CONTENT));
        rewriter.applyChanges(changes);
        
        assertTrue(new File(helper.srcDir, "com/atlassian/test/MyClass.java").exists());
    }
    
    @Test
    public void sourceFileHasContent() throws Exception
    {
        PluginProjectChangeset changes = new PluginProjectChangeset()
            .with(sourceFile(CLASS, SourceFile.SourceGroup.MAIN, CONTENT));
        rewriter.applyChanges(changes);
        
        assertEquals(CONTENT, FileUtils.readFileToString(new File(helper.srcDir, "com/atlassian/test/MyClass.java")));
    }
    
    @Test
    public void testSourceFileIsCreated() throws Exception
    {
        PluginProjectChangeset changes = new PluginProjectChangeset()
            .with(sourceFile(CLASS, SourceFile.SourceGroup.TESTS, CONTENT));
        rewriter.applyChanges(changes);
        
        assertTrue(new File(helper.testDir, "com/atlassian/test/MyClass.java").exists());
    }
    
    @Test
    public void testSourceFileHasContent() throws Exception
    {
        PluginProjectChangeset changes = new PluginProjectChangeset()
            .with(sourceFile(CLASS, SourceFile.SourceGroup.TESTS, CONTENT));
        rewriter.applyChanges(changes);
        
        assertEquals(CONTENT, FileUtils.readFileToString(new File(helper.testDir, "com/atlassian/test/MyClass.java")));
    }
    
    @Test
    public void resourceFileIsCreated() throws Exception
    {
        PluginProjectChangeset changes = new PluginProjectChangeset()
            .with(ResourceFile.resourceFile("templates/test", "template.vm", CONTENT));
        rewriter.applyChanges(changes);
        
        assertTrue(new File(helper.resourcesDir, "templates/test/template.vm").exists());        
    }
    
    @Test
    public void resourceFileHasContent() throws Exception
    {
        PluginProjectChangeset changes = new PluginProjectChangeset()
            .with(ResourceFile.resourceFile("templates/test", "template.vm", CONTENT));
        rewriter.applyChanges(changes);
        
        assertEquals(CONTENT, FileUtils.readFileToString(new File(helper.resourcesDir, "templates/test/template.vm")));
    }
    
    @Test
    public void i18nPropertyFileIsCreatedWithDefaultLocation() throws Exception
    {
        rewriter.applyChanges(changeset().with(i18nString("foo", "bar")));
        
        assertTrue(i18nFile.exists());        
    }
    
    @Test
    public void i18nPropertyFileHasPropertyValue() throws Exception
    {
        rewriter.applyChanges(changeset().with(i18nString("foo", "bar")));

        Properties props = new Properties();
        FileInputStream in = new FileInputStream(i18nFile);
        try
        {
            props.load(in);
        }
        finally
        {
            in.close();
        }
        assertEquals("bar", props.getProperty("foo"));
    }
    
    @Test
    public void duplicateI18nPropertyNameIsNotAdded() throws Exception
    {
        FileUtils.writeStringToFile(i18nFile, "foo=goo\n");
        rewriter.applyChanges(changeset().with(i18nString("foo", "bar")));
        
        assertEquals("foo=goo\n", FileUtils.readFileToString(i18nFile));
    }

    @Test
    public void propertiesAreNotReordered() throws Exception
    {
        FileUtils.writeStringToFile(i18nFile, "foo=goo\nall=lol");
        rewriter.applyChanges(changeset().with(i18nString("gal", "pal"), i18nString("bar", "car")));
        
        assertEquals("foo=goo\nall=lol\ngal=pal\nbar=car\n", FileUtils.readFileToString(i18nFile));
    }

    @Test
    public void i18nPropertyFileIsCreatedWithCustomLocation() throws Exception
    {
        usePluginXml("plugin-with-same-i18n-name.xml");

        rewriter.applyChanges(changeset().with(i18nString("foo", "bar")));
    
        assertTrue(new File(helper.resourcesDir, "nonstandard-location.properties").exists());
    }
}
