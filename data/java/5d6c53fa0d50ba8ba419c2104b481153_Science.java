/*******************************************************************************
 * Copyright 2011 Wolfgang Loeffler.
 * Released under the GPL license: http://www.gnu.org/licenses/gpl.html
 ******************************************************************************/
package com.wolle.reftool4.imports.fromweb;

import java.io.InputStream;

import org.eclipse.core.runtime.IProgressMonitor;

import com.gargoylesoftware.htmlunit.Page;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlMeta;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.wolle.reftool4.imports.ArticleTools;
import com.wolle.reftool4.imports.BibtexScanner;
import com.wolle.reftool4.model.Article;

public class Science extends AbstractImportFromWeb {

	@Override
	public int doimport(String starturl, IProgressMonitor monitor, WebClient webClient) {
		String id = "http://www.sciencemag.org/";
		if (!starturl.startsWith(id)) return ERR_NOMATCH;

		starturl = removeJunkBehind(starturl, "?");

		log.debug("doimportscience: url=" + starturl);
		article = new Article();
		article.setEntrytype("article");
		article.setLinkurl(starturl);
		

//		final WebClient webClient = getWebClient();
		webClient.setJavaScriptEnabled(false);
		
		try {
			monitor.setTaskName("getting main page for " + article);
			HtmlPage mainpage = webClient.getPage(starturl);
			
			//bibtex
			HtmlMeta meta = mainpage.getElementByName("citation_mjid");
			String biburl = "http://www.sciencemag.org/citmgr?type=bibtex&gca=" + meta.getContentAttribute();
			log.debug("biburl=" + biburl);
			monitor.setTaskName("getting bib page...");
			Page bibpage = webClient.getPage(biburl);
			String bibtex = bibpage.getWebResponse().getContentAsString();
			article = BibtexScanner.scanToArticle(bibtex, article);
			monitor.worked(1);

			// pdf
			String pdfurl = ((HtmlMeta) mainpage.getElementByName("citation_pdf_url")).getContentAttribute();
			monitor.setTaskName("getting pdf page for " + article);
			log.debug("pdfurl=" + pdfurl);
			InputStream is = getInputStreamFromURL(webClient, pdfurl);
			file = createPdfFile(ArticleTools.getArticleFilename(article.getTitle()), is);
			monitor.worked(1);
		} catch (Exception e) {
			e.printStackTrace();
			return -1;
		}

		return 0;
	}

}
