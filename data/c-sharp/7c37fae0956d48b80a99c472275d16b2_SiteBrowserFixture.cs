using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MDownloader.Hostings.Common;
using MDownloader.Hostings.Core;
using MDownloader.Hostings.Exceptions;
using MDownloader.Hostings.Providers.FileServe;

#if NUNIT
using Assert = NUnit.Framework.Assert;
using ExpectedExceptionAttribute = NUnit.Framework.ExpectedExceptionAttribute;
using TestClassAttribute = NUnit.Framework.TestFixtureAttribute;
using TestMethodAttribute = NUnit.Framework.TestAttribute;
#else
using Microsoft.VisualStudio.TestTools.UnitTesting;
#endif

namespace MDownloader.Hostings.Tests.Providers.FileServe
{
    [TestClass]
    public sealed class SiteBrowserFixture : SiteBrowserFixtureBase
    {
        private IHosting browser = new SiteBrowser();

        [TestMethod]
        public void CanGetResourceMetadata()
        {
            var context = new DataRetrievingContextMock();
            context.RegisterResource(this.manager.GetFullPath(@"Default.Info"), @"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar");

            var target = new RetrievableTarget { Provider = this.browser.GetInfo().Id, Link = new Uri(@"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar") };
            var metadata = browser.GetResourceMetadata(context, target, new DownloadSpecification(null, null));

            Assert.AreEqual("Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar", metadata.Name);
            Assert.AreEqual(Convert.ToInt64(200 * 1024 * 1024), metadata.Size);
            Assert.IsTrue(metadata.IsSizeApproximate());
        }

        [TestMethod]
        public void CanGetResourceData()
        {
            var data = manager.GetData(@"Default.Data");
            var context = new DataRetrievingContextMock();
            context.RegisterResource(this.manager.GetFullPath(@"Default.Data"), @"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar", @"application/octet-stream", this.OnVerifyData);
            context.RegisterResource(this.manager.GetFullPath(@"Default.Show"), @"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar", this.OnVerifyShow);
            context.RegisterResource(this.manager.GetFullPath(@"Default.Timer"), @"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar", this.OnVerifyTimer);
            context.RegisterResource(this.manager.GetFullPath(@"Default.Check"), @"http://www.fileserve.com/checkReCaptcha.php", this.OnVerifyCaptcha);
            context.RegisterResource(this.manager.GetCaptchaPath(), @"http://www.google.com/recaptcha/api/image?c=03AHJ_VutsVq40pjOWWhTSO8lWThqYi1NDVtSlIYPuWzcWkJhC0jTYRxnxN3XmMRYZRYlZByhBr8SSiykmFInxnuRQKIfMQdPYPyoTzn9MQWbDrVkxlP__R80z50BrEj8J4-j_Ke6xioNzFiBLVnnvThZ9oRy0kGK-Qw", @"application/octet-stream");
            context.RegisterResource(this.manager.GetFullPath(@"Default.Captcha"), @"http://www.google.com/recaptcha/api/challenge?k=6LdSvrkSAAAAAOIwNj-IY-Q-p90hQrLinRIpZBPi&ajax=1&cachestop=0.22067506109428647");
            context.RegisterResource(this.manager.GetFullPath(@"Default.Info"), @"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar", this.OnVerifyRequest);

            var target = new RetrievableTarget { Provider = this.browser.GetInfo().Id, Link = new Uri(@"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar") };
            var result = this.ReadToEnd(browser.GetResourceData(context, target, new DownloadSpecification(null, null)).OpenStream());
            Assert.AreEqual(data, result);
        }

        private bool OnVerifyRequest(IDictionary<string, string> data)
        {
            return data.Count == 0;
        }

        private bool OnVerifyCaptcha(IDictionary<string, string> data)
        {
            return data.ContainsKey("recaptcha_response_field") && data["recaptcha_response_field"] == "PASSCODE"
                && data.ContainsKey("recaptcha_challenge_field") && data["recaptcha_challenge_field"] == "03AHJ_VutsVq40pjOWWhTSO8lWThqYi1NDVtSlIYPuWzcWkJhC0jTYRxnxN3XmMRYZRYlZByhBr8SSiykmFInxnuRQKIfMQdPYPyoTzn9MQWbDrVkxlP__R80z50BrEj8J4-j_Ke6xioNzFiBLVnnvThZ9oRy0kGK-Qw"
                && data.ContainsKey("recaptcha_shortencode_field") && data["recaptcha_shortencode_field"] == "RDasF4t";
        }

        private bool OnVerifyTimer(IDictionary<string, string> data)
        {
            return data.ContainsKey("downloadLink") && data["downloadLink"] == "wait";
        }

        private bool OnVerifyShow(IDictionary<string, string> data)
        {
            return data.ContainsKey("downloadLink") && data["downloadLink"] == "show";
        }

        private bool OnVerifyData(IDictionary<string, string> data)
        {
            return data.ContainsKey("download") && data["download"] == "normal";
        }

        [TestMethod, ExpectedException(typeof(NotFoundException))]
        public void CanDealWithNonexistingResourceDuringDataRequest()
        {
            var context = new DataRetrievingContextMock();
            context.RegisterResource(this.manager.GetFullPath(@"Faked.Info.V1"), @"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar");

            var target = new RetrievableTarget { Provider = this.browser.GetInfo().Id, Link = new Uri(@"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar") };
            browser.GetResourceData(context, target, new DownloadSpecification(null, null));
        }

        [TestMethod, ExpectedException(typeof(NotFoundException))]
        public void CanDealWithNonexistingResourceDuringMetadataRequest()
        {
            var context = new DataRetrievingContextMock();
            context.RegisterResource(this.manager.GetFullPath(@"Faked.Info.V1"), @"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar");

            var target = new RetrievableTarget { Provider = this.browser.GetInfo().Id, Link = new Uri(@"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar") };
            browser.GetResourceMetadata(context, target, new DownloadSpecification(null, null));
        }

        [TestMethod, ExpectedException(typeof(NotFoundException))]
        public void CanDealWithNonexistingResourceDuringDataRequestV2()
        {
            var context = new DataRetrievingContextMock();
            context.RegisterResource(this.manager.GetFullPath(@"Faked.Info.V2"), @"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar");

            var target = new RetrievableTarget { Provider = this.browser.GetInfo().Id, Link = new Uri(@"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar") };
            browser.GetResourceData(context, target, new DownloadSpecification(null, null));
        }

        [TestMethod, ExpectedException(typeof(NotFoundException))]
        public void CanDealWithNonexistingResourceDuringMetadataRequestV2()
        {
            var context = new DataRetrievingContextMock();
            context.RegisterResource(this.manager.GetFullPath(@"Faked.Info.V2"), @"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar");

            var target = new RetrievableTarget { Provider = this.browser.GetInfo().Id, Link = new Uri(@"http://www.fileserve.com/file/RDasF4t/Science.Channel.Through.the.Wormhole.S01E01.Is.There.a.Creator.720p.HDTV.x264-DHD.part1.rar") };
            browser.GetResourceMetadata(context, target, new DownloadSpecification(null, null));
        }

    }
}
