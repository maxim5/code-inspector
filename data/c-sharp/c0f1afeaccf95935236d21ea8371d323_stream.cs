using System.Collections.Generic;
using facebook.Schema;
using System;
using facebook.Utility;

namespace facebook
{
	/// <summary>
	/// Facebook application api methods.
	/// </summary>
	public class stream
	{
		private readonly API _api;
		/// <summary>
		/// Public constructor for facebook.stream
		/// </summary>
		/// <param name="parent">Needs a connected API object for making requests</param>
		public stream(API parent)
		{
			_api = parent;
		}

		/// <summary>
		/// 
		/// </summary>
		/// <returns></returns>
        public string addComment(string post_id, string comment)
		{
            return addComment(0, post_id, comment);
		}
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public string addComment(int uid, string post_id, string comment)
        {
            var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.addComment" } };
            _api.AddOptionalParameter(parameterList, "uid", uid);
            _api.AddOptionalParameter(parameterList, "post_id", post_id);
            _api.AddOptionalParameter(parameterList, "comment", comment);

            var response = _api.SendRequest(parameterList, uid<=0);
            return !string.IsNullOrEmpty(response) ? stream_addComment_response.Parse(response).TypedValue : null;

        }
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public bool addLike(string post_id)
        {
            return addLike(0, post_id);
        }
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public bool addLike(int uid, string post_id)
        {
            var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.addLike" } };
            _api.AddOptionalParameter(parameterList, "uid", uid);
            _api.AddOptionalParameter(parameterList, "post_id", post_id);

            var response = _api.SendRequest(parameterList, uid<=0);
            return string.IsNullOrEmpty(response) || stream_addLike_response.Parse(response).TypedValue;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public stream_data get(int viewer_id, List<string> source_ids, DateTime start_time, DateTime end_time, int limit, string filter_key)
        {
            var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.get" } };
            _api.AddOptionalParameter(parameterList, "viewer_id", viewer_id);
            _api.AddList(parameterList, "source_ids", source_ids);
            _api.AddOptionalParameter(parameterList, "start_time", DateHelper.ConvertDateToDouble(start_time));
            _api.AddOptionalParameter(parameterList, "end_time", DateHelper.ConvertDateToDouble(end_time));
            _api.AddOptionalParameter(parameterList, "limit", limit);
            _api.AddOptionalParameter(parameterList, "filter_key", filter_key);
            //var list = new List<string>();
            //if (filters != null)
            //{
            //    foreach (var item in filters)
            //    {
            //        var dict = new Dictionary<string, string>{
            //            {"filter_key", item.filter_key},
            //            {"icon_url", item.icon_url},
            //            {"is_visible", item.is_visible.ToString()},
            //            {"name", item.name},
            //            {"rank", item.rank.ToString()},
            //            {"uid", item.uid.ToString()},
            //            {"value", item.value.ToString()},
            //        };
            //        list.Add(JSONHelper.ConvertToJSONAssociativeArray(dict));
            //    }
            //}
            //_api.AddJSONArray(parameterList, "filters", list);            

            var response = _api.SendRequest(parameterList, true);
            return !string.IsNullOrEmpty(response) ? stream_get_response.Parse(response).Content : null;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public IList<comment> getComments(string post_id)
        {
            var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.getComments" } };
            _api.AddRequiredParameter(parameterList, "post_id", post_id);

            var response = _api.SendRequest(parameterList);
            return !string.IsNullOrEmpty(response) ? stream_getComments_response.Parse(response).comment : null;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public IList<stream_filter> getFilters(int uid)
        {
            var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.getFilters" } };
            _api.AddOptionalParameter(parameterList, "uid", uid);

            var response = _api.SendRequest(parameterList, uid<=0);
            return !string.IsNullOrEmpty(response) ? stream_getFilters_response.Parse(response).stream_filter : null;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public string publish(string message)
        {
            return publish(message, null, null, null, 0);
        }
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public string publish(string message, attachment attachment, IList<action_link> action_links, string target_id, int uid)
        {
            var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.publish" } };
            _api.AddOptionalParameter(parameterList, "message", message);
            if (attachment != null)
            {
                var mediaList = new List<string>();
                var cat = new Dictionary<string, string>{
                    {"text", attachment.properties.category.text},
                    {"href", attachment.properties.category.href}
                };
                var prop = new Dictionary<string, string>{
                    {"category", JSONHelper.ConvertToJSONAssociativeArray(cat)},
                    {"ratings", attachment.properties.ratings}
                };

                if(attachment.media != null)
                {
                    foreach (var item in attachment.media)
                    {
                        var media = new Dictionary<string, string>{
                               {"type", item.type.ToString()}
                                };
                        if (item.type == attachment_media_type.image)
                        {
                            var image = item as attachment_media_image;
                            media.Add("src", image.src);
                            media.Add("href", image.href);
                        }
                        else if (item.type == attachment_media_type.flash)
                        {
                            var flash = item as attachment_media_flash;
                            media.Add("swfsrc", flash.swfsrc);
                            media.Add("imgsrc", flash.imgsrc);
                            media.Add("width", flash.width.ToString());
                            media.Add("height", flash.height.ToString());
                            media.Add("expanded_width", flash.expanded_width.ToString());
                            media.Add("expanded_height", flash.expanded_height.ToString());
                        }
                        else if (item.type == attachment_media_type.mp3)
                        {
                            var mp3 = item as attachment_media_mp3;
                            media.Add("src", mp3.src);
                            media.Add("title", mp3.title);
                            media.Add("artist", mp3.artist);
                            media.Add("album", mp3.album);
                        }
                        else
                        {
                            var video = item as attachment_media_video;
                            media.Add("video_src", video.video_src);
                            media.Add("preview_img", video.preview_img);
                            media.Add("video_link", video.video_link);
                            media.Add("video_title", video.video_title);
                        }
                        mediaList.Add(JSONHelper.ConvertToJSONAssociativeArray(media));
                    }

                }
                var dict = new Dictionary<string, string>{
                    {"name", attachment.name},
                    {"href", attachment.href},
                    {"caption", attachment.caption},
                    {"description", attachment.description},
                    {"properties", JSONHelper.ConvertToJSONAssociativeArray(prop)},
                    {"media", JSONHelper.ConvertToJSONArray(mediaList)},
                    {"latitude", attachment.latitude},
                    {"longitude", attachment.longitude}
                };
                _api.AddOptionalParameter(parameterList, "attachment", JSONHelper.ConvertToJSONAssociativeArray(dict));
            }
            if (action_links != null)
            {
                var list = new List<string>();
                foreach (var item in action_links)
                {
                    var dict = new Dictionary<string, string>{
                    {"text", item.text},
                    {"href", item.href}
                };
                    list.Add(JSONHelper.ConvertToJSONAssociativeArray(dict));
                }
                _api.AddJSONArray(parameterList, "action_links", list);

            }
            _api.AddOptionalParameter(parameterList, "target_id", target_id);
            _api.AddOptionalParameter(parameterList, "uid", uid);

            var response = _api.SendRequest(parameterList, uid<=0);
            return !string.IsNullOrEmpty(response) ? stream_publish_response.Parse(response).TypedValue : null;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public bool remove(string post_id)
        {
            return remove(0, post_id);
        }
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public bool remove(int uid, string post_id)
        {
            var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.remove" } };
            _api.AddOptionalParameter(parameterList, "uid", uid);
            _api.AddRequiredParameter(parameterList, "post_id", post_id);

            var response = _api.SendRequest(parameterList, uid<=0);
            return string.IsNullOrEmpty(response) || stream_remove_response.Parse(response).TypedValue;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public bool removeComment(string comment_id)
        {
            return removeComment(0, comment_id);
        }
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public bool removeComment(int uid, string comment_id)
        {
            var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.removeComment" } };
            _api.AddOptionalParameter(parameterList, "uid", uid);
            _api.AddRequiredParameter(parameterList, "comment_id", comment_id);

            var response = _api.SendRequest(parameterList, uid<=0);
            return string.IsNullOrEmpty(response) || stream_removeComment_response.Parse(response).TypedValue;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public bool removeLike(string post_id)
        {
            return removeLike(0, post_id);
        }
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public bool removeLike(int uid, string post_id)
        {
            var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.removeLike" } };
            _api.AddOptionalParameter(parameterList, "uid", uid);
            _api.AddRequiredParameter(parameterList, "post_id", post_id);

            var response = _api.SendRequest(parameterList, uid<=0);
            return string.IsNullOrEmpty(response) || stream_removeLike_response.Parse(response).TypedValue;
        }
    }
    public class attachment
    {
        public string name { get; set; }
        public string href { get; set; }
        public string caption { get; set; }
        public string description { get; set; }
        public attachment_property properties { get; set; }
        public List<attachment_media> media { get; set; }
        public string latitude { get; set; }
        public string longitude { get; set; }
    }
    public class attachment_property
    {
        public attachment_category category { get; set; }
        public string ratings { get; set; }
    }
    public class attachment_category
    {
        public string href { get; set; }
        public string text { get; set; }

    }
    public class attachment_media
    {
        public attachment_media_type type { get; set; }
    }
    public class attachment_media_image : attachment_media
    {
        public attachment_media_image()
        {
            this.type = attachment_media_type.image;
        }
        public string src { get; set; }
        public string href { get; set; }
    }
    public class attachment_media_flash : attachment_media
    {
        public attachment_media_flash()
        {
            this.type = attachment_media_type.flash;
        }
        public string swfsrc { get; set; }
        public string imgsrc { get; set; }
        public int width { get; set; }
        public int height { get; set; }
        public int expanded_width { get; set; }
        public int expanded_height { get; set; }
    }

    public class attachment_media_mp3 : attachment_media
    {
        public attachment_media_mp3()
        {
            this.type = attachment_media_type.mp3;
        }
        public string src { get; set; }
        public string title { get; set; }
        public string artist { get; set; }
        public string album { get; set; }
    }
    public class attachment_media_video: attachment_media
    {
        public attachment_media_video()
        {
            this.type = attachment_media_type.video;
        }
        public string video_src { get; set; }
        public string preview_img { get; set; }
        public string video_link { get; set; }
        public string video_title { get; set; }
    }

    public enum attachment_media_type
    {
        image,flash,mp3,video
    }
}
