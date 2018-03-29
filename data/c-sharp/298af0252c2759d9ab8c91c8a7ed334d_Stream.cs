using System;
using System.Collections.Generic;
using Facebook.Schema;
using Facebook.Session;
using Facebook.Utility;

namespace Facebook.Rest
{
	/// <summary>
	/// Facebook Stream API methods.
	/// </summary>
	public class Stream : RestBase
	{
		#region Methods

		#region Constructor

		/// <summary>
		/// Public constructor for facebook.Stream
		/// </summary>
		/// <param name="session">Needs a connected Facebook Session object for making requests</param>
		public Stream(FacebookSession session)
			: base(session)
		{
		}

		#endregion Constructor

		#region Public Methods

#if !SILVERLIGHT

		#region Synchronous Methods

        /// <summary>
        /// This method adds a comment to a post that was already published to a user's Wall.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        /// var result = api.Stream.AddComment(Constants.PostId1, "adding a comment");
        /// </code>
        /// </example>
        /// <param name="postId">The ID for the post to which you're adding the comment.</param>
        /// <param name="comment">The text of the comment. This is a plain text parameter only; you cannot format the comment with HTML or FBML.</param>
        /// <returns>This call returns a comment_id if the comment was added successfully, or an error code if the call was unsuccessful.</returns>
        public string AddComment(string postId, string comment)
		{
            return AddComment(0, postId, comment);
		}

        /// <summary>
        /// This method adds a comment to a post that was already published to a user's Wall.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        /// var result = api.Stream.AddComment(Constants.UserId, Constants.PostId1, "adding a comment");
        /// </code>
        /// </example>
        /// <param name="uid">The user ID of the user adding the comment. If this parameter is not specified, then it defaults to the session user. Note: This parameter applies only to Web applications.  Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <param name="postId">The ID for the post to which you're adding the comment.</param>
        /// <param name="comment">The text of the comment. This is a plain text parameter only; you cannot format the comment with HTML or FBML.</param>
        /// <returns>This call returns a comment_id if the comment was added successfully, or an error code if the call was unsuccessful.</returns>
        public string AddComment(long uid, string postId, string comment)
		{
            return AddComment(uid, postId, comment, false, null, null);
		}

        /// <summary>
        /// This method lets a user add a like to any post the user can see. A user can like each post only once.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        /// var result = api.Stream.AddLike(Constants.PostId1);
        /// </code>
        /// </example>
        /// <param name="postId">The ID of the post.</param>
        /// <returns>This method returns true on success, or false and an error code if it fails.</returns>
        public bool AddLike(string postId)
		{
            return AddLike(0, postId);
		}

        /// <summary>
        /// This method lets a user add a like to any post the user can see. A user can like each post only once.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        /// var result = api.Stream.AddLike(Constants.UserId, Constants.PostId1);
        /// </code>
        /// </example>
        /// <param name="uid">The user ID of the user who likes the post. If this parameter is not specified, then it defaults to the session user. Note: This parameter applies only to Web applications.  Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <param name="postId">The ID of the post.</param>
        /// <returns>This method returns true on success, or false and an error code if it fails.</returns>
        public bool AddLike(long uid, string postId)
		{
            return AddLike(uid, postId, false, null, null);
		}

        /// <summary>
        /// This method returns an object that contains the stream from the perspective of a specific viewer -- a user or a Facebook Page.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        /// var result = api.Stream.Get(null, DateTime.Now.AddYears(-2), DateTime.Now, 0);
        /// </code>
        /// </example>
        /// <param name="sourceIds">An array containing all the stream data for the user profiles and Pages connected to the viewer_id. You can filter the stream to include posts by the IDs you specify here. (Default value is all connections of the viewer.)</param>
        /// <param name="startTime">The earliest DateTime for which to retrieve posts from the stream. The start_time uses the updated_time field in the stream (FQL) table as the baseline for determining the earliest time for which to get the stream.</param>
        /// <param name="endTime">The latest DateTime for which to retrieve posts from the stream. The end_time uses the updated_time field in the stream (FQL) table as the baseline for determining the latest time for which to get the stream.</param>
        /// <param name="limit">The total number of posts to return. (Default value is 30 posts.)</param>
        /// <returns>This method returns a stream_data object containing the following arrays:  posts, which is an array of post data, containing the fields defined by the stream (FQL) table.  profiles, which is an array of profile information, containing the fields defined by the profile (FQL) table.  albums, which is an array of album information, containing the field as defined by the album (FQL) table.</returns>
		public stream_data Get(List<long> sourceIds, DateTime? startTime, DateTime? endTime, int? limit)
		{
			return Get(Session.UserId, sourceIds, startTime, endTime, limit, string.Empty);
		}

        /// <summary>
        /// This method returns an object that contains the stream from the perspective of a specific viewer -- a user or a Facebook Page.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        /// var result = api.Stream.Get(0, null, DateTime.Now.AddYears(-2), DateTime.Now, 0, null);
        /// </code>
        /// </example>
        /// <param name="viewerId">The user ID for whom you are fetching stream data. You can pass 0 for this parameter to retrieve publicly viewable information.</param>
        /// <param name="sourceIds">An array containing all the stream data for the user profiles and Pages connected to the viewer_id. You can filter the stream to include posts by the IDs you specify here. (Default value is all connections of the viewer.)</param>
        /// <param name="startTime">The earliest DateTime for which to retrieve posts from the stream. The start_time uses the updated_time field in the stream (FQL) table as the baseline for determining the earliest time for which to get the stream.</param>
        /// <param name="endTime">The latest DateTime for which to retrieve posts from the stream. The end_time uses the updated_time field in the stream (FQL) table as the baseline for determining the latest time for which to get the stream.</param>
        /// <param name="limit">The total number of posts to return. (Default value is 30 posts.)</param>
        /// <param name="filter_key">A filter associated with the user. Filters get returned by stream.getFilters or the stream_filter FQL table. To filter for stream posts from your application, look for a filter with a filter_key set to app_YOUR_APPLICATION_ID.</param>
        /// <returns>This method returns a stream_data object containing the following arrays:  posts, which is an array of post data, containing the fields defined by the stream (FQL) table.  profiles, which is an array of profile information, containing the fields defined by the profile (FQL) table.  albums, which is an array of album information, containing the field as defined by the album (FQL) table.</returns>
        public stream_data Get(long viewerId, List<long> sourceIds, DateTime? startTime, DateTime? endTime, int? limit, string filter_key)
		{
            return Get(viewerId, sourceIds, startTime, endTime, limit, filter_key, false, null, null);
		}

        /// <summary>
        /// This method returns all comments associated with a post in a user's stream. This method returns comments only if the user who owns the post (that is, the user published the post to his or her profile) has authorized your application.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        /// var result = api.Stream.GetComments(Constants.PostId1);
        /// </code>
        /// </example>
        /// <param name="post_id">The ID for the post for which you're retrieving the comments.</param>
        /// <returns>This method returns a List of comments, where each comment contains the fields from the comment (FQL) table.</returns>
		public IList<comment> GetComments(string post_id)
		{
			return GetComments(post_id, false, null, null);
		}

        /// <summary>
        /// This method returns any filters a user has specified for his or her home page stream.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        /// var result = api.Stream.GetFilters();
        /// </code>
        /// </example>
        /// <returns>This method returns a List of data containing the fields from the stream_filter_(FQL) table.</returns>
		public IList<stream_filter> GetFilters()
		{
			return GetFilters(0);
		}

        /// <summary>
        /// This method returns any filters a user has specified for his or her home page stream.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        /// var result = api.Stream.GetFilters(Constants.UserId);
        /// </code>
        /// </example>
        /// <param name="uid">The user ID for the user whose stream filters you are returning.  Note: This parameter applies only to Web applications. Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <returns>This method returns a List of data containing the fields from the stream_filter_(FQL) table.</returns>
        public IList<stream_filter> GetFilters(long uid)
		{
			return GetFilters(uid, false, null, null);
		}

        /// <summary>
        /// This method publishes a post into the stream on the user's Wall and News Feed. This post also appears in the user's friends' streams (their News Feeds).
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        /// var result = api.Stream.Publish("Publishing to stream");
        /// </code>
        /// </example>
        /// <param name="message">The message the user enters for the post at the time of publication.</param>
        /// <returns>This call returns a post_id string containing the ID of the stream item upon success. If the call fails, it returns an error code instead.</returns>
        public string Publish(string message)
		{
			return Publish(message, null, null, null, 0);
		}

        /// <summary>
        /// This method publishes a post into the stream on the user's Wall and News Feed. This post also appears in the user's friends' streams (their News Feeds).
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        /// attachment attachment = new attachment();
        /// attachment.caption = "facebook.com";
        /// attachment.name = "Publish Test";
        /// attachment.href = "http://www.facebook.com";
        /// attachment.description = "a sample description";
        /// 
        /// attachment.properties = new attachment_property()
        /// {
        ///     category = new attachment_category()
        ///     {
        ///         href = "http://www.facebook.com/mycategory/sample",
        ///         text = "sample category"
        ///     },
        ///     ratings = "5 stars"
        /// };
        ///
        /// attachment.media = new List&lt;attachment_media&gt;(){new attachment_media_image()
        ///                         {
        ///                             src = "http://facebook.com/myapp/logo.jpg",
        ///                             href = "http://www.facebook.com/myapp"
        ///                         }};
        /// 
        /// var result = api.Stream.Publish("Publishing to stream with attachment", attachment, null, null, 0);
        /// </code>
        /// </example>
        /// <param name="message">The message the user enters for the post at the time of publication.</param>
        /// <param name="attachment">An object containing the text of the post, relevant links, a media type (image, video, mp3, flash), as well as any other key/value pairs you may want to add. See Facebook API for more details.  Note: If you want to use this call to update a user's status, don't pass an attachment; the content of the message parameter will become the user's new status and will appear at the top of the user's profile.</param>
        /// <param name="actionLinks">A List of action link objects, containing the link text and a hyperlink.</param>
        /// <param name="target_id">The ID of the user or the Page where you are publishing the content. If you specify a target_id, the post appears on the Wall of the target user, not on the Wall of the user who published the post. This mimics the action of posting on a friend's Wall on Facebook itself.</param>
        /// <param name="uid">The user ID or Page ID of the user or Page publishing the post. If this parameter is not specified, then it defaults to the session user.  Note: This parameter applies only to Web applications.  Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <returns>This call returns a post_id string containing the ID of the stream item upon success. If the call fails, it returns an error code instead.</returns>
        public string Publish(string message, attachment attachment, IList<action_link> actionLinks, string target_id, long uid)
		{
            return Publish(message, attachment, actionLinks, target_id, uid, false, null, null);
		}

        /// <summary>
        /// This method removes a post from a user's Wall. The post also gets removed from the user's and the user's friends' News Feeds. Your application may only remove posts that were created through it.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        /// api.Session.UserId = Constants.UserId;
        /// var actual = api.Stream.Remove(Constants.PostId2);
        /// </code>
        /// </example>
        /// <returns>This call returns true if the post was removed, or false and an error code if the post was not removed.</returns>
        public bool Remove(string post_id)
		{
			return Remove(0, post_id);
		}

        /// <summary>
        /// This method removes a post from a user's Wall. The post also gets removed from the user's and the user's friends' News Feeds. Your application may only remove posts that were created through it.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        /// api.Session.UserId = Constants.UserId;
        /// var actual = api.Stream.Remove(Constants.UserId, Constants.PostId2);
        /// </code>
        /// </example>
        /// <param name="uid">The user ID of the user publishing the post. If this parameter is not specified, then it defaults to the session user.  Note: This parameter applies only to Web applications. Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <param name="postId">The ID for the post you want to remove.</param>
        /// <returns>This call returns true if the post was removed, or false and an error code if the post was not removed.</returns>
        public bool Remove(long uid, string postId)
		{
			return Remove(uid, postId, false, null, null);
		}

        /// <summary>
        /// This method removes a comment from a post.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        /// api.Session.UserId = Constants.UserId;
        /// var actual = api.Stream.RemoveComment(Constants.CommentId);
        /// </code>
        /// </example>
        /// <returns>This call returns true if the comment was removed, or false and an error code if the comment was not removed.</returns>
		public bool RemoveComment(string comment_id)
		{
			return RemoveComment(0, comment_id);
		}

        /// <summary>
        /// This method removes a comment from a post.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        /// api.Session.UserId = Constants.UserId;
        /// var actual = api.Stream.RemoveComment(Constants.UserId, Constants.CommentId);
        /// </code>
        /// </example>
        /// <param name="uid">The user ID of the user who made the comment. If this parameter is not specified, then it defaults to the session user. Note: This parameter applies only to Web applications.  Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <param name="commentId">The ID for the comment you want to remove.</param>
        /// <returns>This call returns true if the comment was removed, or false and an error code if the comment was not removed.</returns>
        public bool RemoveComment(long uid, string commentId)
		{
            return RemoveComment(uid, commentId, false, null, null);
		}

        /// <summary>
        /// This method removes a like a user added to a post.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        /// api.Session.UserId = Constants.UserId;
        /// var actual = api.Stream.RemoveLike(Constants.PostId1);
        /// </code>
        /// </example>
        /// <param name="postId">The ID of the post.</param>
        /// <returns>This method returns true on success, or false and an error code if it fails.</returns>
        public bool RemoveLike(string postId)
		{
            return RemoveLike(0, postId);
		}

        /// <summary>
        /// This method removes a like a user added to a post.
        /// </summary>
        /// <example>
        /// <code>
        /// Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        /// api.Session.UserId = Constants.UserId;
        /// var actual = api.Stream.RemoveLike(Constants.UserId, Constants.PostId1);
        /// </code>
        /// </example>
        /// <param name="uid">The user ID of the user who liked the post. If this parameter is not specified, then it defaults to the session user. Note: This parameter applies only to Web applications.  Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <param name="postId">The ID of the post.</param>
        /// <returns>This method returns true on success, or false and an error code if it fails.</returns>
        public bool RemoveLike(long uid, string postId)
		{
            return RemoveLike(uid, postId, false, null, null);
		}

		#endregion

#endif

		#region Asynchronous Methods

        /// <summary>
        /// This method returns an object that contains the stream from the perspective of a specific viewer -- a user or a Facebook Page.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        ///     api.Stream.GetAsync(null, DateTime.Now.AddYears(-2), DateTime.Now, 0, AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(stream_data result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="sourceIds">An array containing all the stream data for the user profiles and Pages connected to the viewer_id. You can filter the stream to include posts by the IDs you specify here. (Default value is all connections of the viewer.)</param>
        /// <param name="startTime">The earliest DateTime for which to retrieve posts from the stream. The start_time uses the updated_time field in the stream (FQL) table as the baseline for determining the earliest time for which to get the stream.</param>
        /// <param name="endTime">The latest DateTime for which to retrieve posts from the stream. The end_time uses the updated_time field in the stream (FQL) table as the baseline for determining the latest time for which to get the stream.</param>
        /// <param name="limit">The total number of posts to return. (Default value is 30 posts.)</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This method returns a stream_data object containing the following arrays:  posts, which is an array of post data, containing the fields defined by the stream (FQL) table.  profiles, which is an array of profile information, containing the fields defined by the profile (FQL) table.  albums, which is an array of album information, containing the field as defined by the album (FQL) table.</returns>
        public void GetAsync(List<long> sourceIds, DateTime? startTime, DateTime? endTime, int? limit, GetCallback callback, Object state)
		{
			GetAsync(Session.UserId, sourceIds, startTime, endTime, limit, string.Empty, callback, state);
		}

        /// <summary>
        /// This method returns an object that contains the stream from the perspective of a specific viewer -- a user or a Facebook Page.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        ///     api.Stream.GetAsync(0, null, DateTime.Now.AddYears(-2), DateTime.Now, 0, null, AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(stream_data result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="viewerId">The user ID for whom you are fetching stream data. You can pass 0 for this parameter to retrieve publicly viewable information.</param>
        /// <param name="sourceIds">An array containing all the stream data for the user profiles and Pages connected to the viewer_id. You can filter the stream to include posts by the IDs you specify here. (Default value is all connections of the viewer.)</param>
        /// <param name="startTime">The earliest DateTime for which to retrieve posts from the stream. The start_time uses the updated_time field in the stream (FQL) table as the baseline for determining the earliest time for which to get the stream.</param>
        /// <param name="endTime">The latest DateTime for which to retrieve posts from the stream. The end_time uses the updated_time field in the stream (FQL) table as the baseline for determining the latest time for which to get the stream.</param>
        /// <param name="limit">The total number of posts to return. (Default value is 30 posts.)</param>
        /// <param name="filter_key">A filter associated with the user. Filters get returned by stream.getFilters or the stream_filter FQL table. To filter for stream posts from your application, look for a filter with a filter_key set to app_YOUR_APPLICATION_ID.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This method returns a stream_data object containing the following arrays:  posts, which is an array of post data, containing the fields defined by the stream (FQL) table.  profiles, which is an array of profile information, containing the fields defined by the profile (FQL) table.  albums, which is an array of album information, containing the field as defined by the album (FQL) table.</returns>
        public void GetAsync(long viewerId, List<long> sourceIds, DateTime? startTime, DateTime? endTime, int? limit, string filter_key, GetCallback callback, Object state)
		{
			Get(viewerId, sourceIds, startTime, endTime, limit, filter_key, true, callback, state);
		}

        /// <summary>
        /// This method returns any filters a user has specified for his or her home page stream.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        ///     api.Stream.GetFiltersAsync(AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(IList&lt;stream_filter&gt; result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This method returns a List of data containing the fields from the stream_filter_(FQL) table.</returns>
        public void GetFiltersAsync(GetFiltersCallback callback, Object state)
		{
			GetFiltersAsync(0, callback, state);
		}

        /// <summary>
        /// This method returns any filters a user has specified for his or her home page stream.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        ///     api.Stream.GetFiltersAsync(Constants.UserId, AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(IList&lt;stream_filter&gt; result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="uid">The user ID for the user whose stream filters you are returning.  Note: This parameter applies only to Web applications. Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This method returns a List of data containing the fields from the stream_filter_(FQL) table.</returns>
        public void GetFiltersAsync(long uid, GetFiltersCallback callback, Object state)
		{
			GetFilters(uid, true, callback, state);
		}

        /// <summary>
        /// This method returns all comments associated with a post in a user's stream. This method returns comments only if the user who owns the post (that is, the user published the post to his or her profile) has authorized your application.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        ///     api.Stream.GetCommentsAsync(Constants.PostId1, AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(IList&lt;comment&gt; result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="post_id">The ID for the post for which you're retrieving the comments.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This method returns a List of comments, where each comment contains the fields from the comment (FQL) table.</returns>
        public void GetCommentsAsync(string post_id, GetCommentsCallback callback, Object state)
		{
            GetComments(post_id, true, callback, state);
		}

        /// <summary>
        /// This method publishes a post into the stream on the user's Wall and News Feed. This post also appears in the user's friends' streams (their News Feeds).
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        ///     api.Stream.PublishAsync("Publishing to stream with attachment", AsyncDemoCompleted, null); 
        /// }
        ///
        /// private static void AsyncDemoCompleted(string result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="message">The message the user enters for the post at the time of publication.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This call returns a post_id string containing the ID of the stream item upon success. If the call fails, it returns an error code instead.</returns>
        public void PublishAsync(string message, PublishCallback callback, Object state)
		{
			PublishAsync(message, null, null, null, 0, callback, state);
		}

        /// <summary>
        /// This method publishes a post into the stream on the user's Wall and News Feed. This post also appears in the user's friends' streams (their News Feeds).
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        ///     attachment attachment = new attachment();
        ///     attachment.caption = "facebook.com";
        ///     attachment.name = "Publish Test";
        ///     attachment.href = "http://www.facebook.com";
        ///     attachment.description = "a sample description";
        ///
        ///     attachment.properties = new attachment_property()
        ///     {
        ///         category = new attachment_category()
        ///         {
        ///             href = "http://www.facebook.com/mycategory/sample",
        ///             text = "sample category"
        ///         },
        ///         ratings = "5 stars"
        ///     };
        ///
        ///     attachment.media = new List&lt;attachment_media&gt;(){new attachment_media_image()
        ///                             {
        ///                                 src = "http://facebook.com/myapp/logo.jpg",
        ///                                 href = "http://www.facebook.com/myapp"
        ///                             }};
        ///
        ///     api.Stream.PublishAsync("Publishing to stream with attachment", attachment, null, null, 0, AsyncDemoCompleted, null); 
        /// }
        ///
        /// private static void AsyncDemoCompleted(string result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="message">The message the user enters for the post at the time of publication.</param>
        /// <param name="attachment">An object containing the text of the post, relevant links, a media type (image, video, mp3, flash), as well as any other key/value pairs you may want to add. See Facebook API for more details.  Note: If you want to use this call to update a user's status, don't pass an attachment; the content of the message parameter will become the user's new status and will appear at the top of the user's profile.</param>
        /// <param name="actionLinks">A List of action link objects, containing the link text and a hyperlink.</param>
        /// <param name="target_id">The ID of the user or the Page where you are publishing the content. If you specify a target_id, the post appears on the Wall of the target user, not on the Wall of the user who published the post. This mimics the action of posting on a friend's Wall on Facebook itself.</param>
        /// <param name="uid">The user ID or Page ID of the user or Page publishing the post. If this parameter is not specified, then it defaults to the session user.  Note: This parameter applies only to Web applications.  Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This call returns a post_id string containing the ID of the stream item upon success. If the call fails, it returns an error code instead.</returns>
        public void PublishAsync(string message, attachment attachment, IList<action_link> actionLinks, string target_id, long uid, PublishCallback callback, Object state)
		{
            Publish(message, attachment, actionLinks, target_id, uid, true, callback, state);
		}

        /// <summary>
        /// This method adds a comment to a post that was already published to a user's Wall.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        ///     api.Stream.AddCommentAsync(Constants.PostId1, "adding a comment (async)", AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(string result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="postId">The ID for the post to which you're adding the comment.</param>
        /// <param name="comment">The text of the comment. This is a plain text parameter only; you cannot format the comment with HTML or FBML.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This call returns a comment_id if the comment was added successfully, or an error code if the call was unsuccessful.</returns>
        public void AddCommentAsync(string postId, string comment, AddCommentCallback callback, Object state)
		{
			AddCommentAsync(0, postId, comment, callback, state);
		}

        /// <summary>
        /// This method adds a comment to a post that was already published to a user's Wall.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        ///     api.Stream.AddCommentAsync(Constants.UserId, Constants.PostId1, "adding a comment (async)", AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(string result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="uid">The user ID of the user adding the comment. If this parameter is not specified, then it defaults to the session user. Note: This parameter applies only to Web applications.  Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <param name="postId">The ID for the post to which you're adding the comment.</param>
        /// <param name="comment">The text of the comment. This is a plain text parameter only; you cannot format the comment with HTML or FBML.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This call returns a comment_id if the comment was added successfully, or an error code if the call was unsuccessful.</returns>
		public void AddCommentAsync(long uid, string postId, string comment, AddCommentCallback callback, Object state)
		{
			AddComment(uid, postId, comment, true, callback, state);
		}

        /// <summary>
        /// This method removes a comment from a post.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        ///     api.Session.UserId = Constants.UserId;
        ///     api.Stream.RemoveCommentAsync(Constants.CommentId, AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(bool result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="commentId">The ID for the comment you want to remove.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This call returns true if the comment was removed, or false and an error code if the comment was not removed.</returns>
		public void RemoveCommentAsync(string commentId, RemoveCommentCallback callback, Object state)
		{
			RemoveCommentAsync(0, commentId, callback, state);
		}

        /// <summary>
        /// This method removes a comment from a post.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        ///     api.Session.UserId = Constants.UserId;
        ///     api.Stream.RemoveCommentAsync(Constants.UserId, Constants.CommentId, AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(bool result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="uid">The user ID of the user who made the comment. If this parameter is not specified, then it defaults to the session user. Note: This parameter applies only to Web applications.  Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <param name="commentId">The ID for the comment you want to remove.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This call returns true if the comment was removed, or false and an error code if the comment was not removed.</returns>
		public void RemoveCommentAsync(long uid, string commentId, RemoveCommentCallback callback, Object state)
		{
			RemoveComment(uid, commentId, true, callback, state);
		}

        /// <summary>
        /// This method removes a post from a user's Wall. The post also gets removed from the user's and the user's friends' News Feeds. Your application may only remove posts that were created through it.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        ///     api.Session.UserId = Constants.UserId;
        ///     api.Stream.RemoveAsync(Constants.PostId2, AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(bool result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="postId">The ID for the post you want to remove.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This call returns true if the post was removed, or false and an error code if the post was not removed.</returns>
        public void RemoveAsync(string postId, RemoveCallback callback, Object state)
		{
			RemoveAsync(0, postId, callback, state);
		}

        /// <summary>
        /// This method removes a post from a user's Wall. The post also gets removed from the user's and the user's friends' News Feeds. Your application may only remove posts that were created through it.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new DesktopSession(Constants.ApplicationKey, Constants.ApplicationSecret, Constants.SessionSecret, Constants.SessionKey));
        ///     api.Session.UserId = Constants.UserId;
        ///     api.Stream.RemoveAsync(Constants.UserId, Constants.PostId2, AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(bool result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="uid">The user ID of the user publishing the post. If this parameter is not specified, then it defaults to the session user.  Note: This parameter applies only to Web applications. Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <param name="postId">The ID for the post you want to remove.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This call returns true if the post was removed, or false and an error code if the post was not removed.</returns>
        public void RemoveAsync(long uid, string postId, RemoveCallback callback, Object state)
		{
            Remove(uid, postId, true, callback, state);
		}

        /// <summary>
        /// This method lets a user add a like to any post the user can see. A user can like each post only once.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        ///     api.Stream.AddLikeAsync(Constants.PostId1, AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(bool result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="postId">The ID of the post.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This method returns true on success, or false and an error code if it fails.</returns>
        public void AddLikeAsync(string postId, AddLikeCallback callback, Object state)
		{
			AddLikeAsync(Session.UserId, postId, callback, state);
		}

        /// <summary>
        /// This method lets a user add a like to any post the user can see. A user can like each post only once.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        ///     api.Stream.AddLikeAsync(Constants.UserId, Constants.PostId1, AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(bool result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="uid">The user ID of the user who likes the post. If this parameter is not specified, then it defaults to the session user. Note: This parameter applies only to Web applications.  Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <param name="postId">The ID of the post.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This method returns true on success, or false and an error code if it fails.</returns>
        public void AddLikeAsync(long uid, string postId, AddLikeCallback callback, Object state)
		{
            AddLike(uid, postId, true, callback, state);
		}

        /// <summary>
        /// This method removes a like a user added to a post.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        ///     api.Session.UserId = Constants.UserId;
        ///     api.Stream.RemoveLikeAsync(Constants.PostId1, AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(bool result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
        /// </code>
        /// </example>
        /// <param name="postId">The ID of the post.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This method returns true on success, or false and an error code if it fails.</returns>
        public void RemoveLikeAsync(string postId, RemoveLikeCallback callback, Object state)
		{
			RemoveLike(Session.UserId, postId, true, callback, state);
		}

        /// <summary>
        /// This method removes a like a user added to a post.
        /// </summary>
        /// <example>
        /// <code>
        /// private static void RunDemoAsync()
        /// {
        ///     Api api = new Api(new FBMLCanvasSession(Constants.WebApplicationKey, Constants.WebSecret));
        ///     api.Session.UserId = Constants.UserId;
        ///     api.Stream.RemoveLikeAsync(Constants.UserId, Constants.PostId1, AsyncDemoCompleted, null);
        /// }
        ///
        /// private static void AsyncDemoCompleted(bool result, Object state, FacebookException e)
        /// {
        ///     var actual = result;
        /// }
         /// </code>
        /// </example>
        /// <param name="uid">The user ID of the user who liked the post. If this parameter is not specified, then it defaults to the session user. Note: This parameter applies only to Web applications.  Facebook ignores this parameter if it is passed by a desktop application.</param>
        /// <param name="postId">The ID of the post.</param>
        /// <param name="callback">The AsyncCallback delegate</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>        
        /// <returns>This method returns true on success, or false and an error code if it fails.</returns>
        public void RemoveLikeAsync(long uid, string postId, RemoveLikeCallback callback, Object state)
		{
            RemoveLike(uid, postId, true, callback, state);
		}

		#endregion

		#endregion Public Methods

		#region Private Methods

		private string AddComment(long uid, string post_id, string comment, bool isAsync, AddCommentCallback callback, Object state)
		{
			var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.addComment" } };
			Utilities.AddOptionalParameter(parameterList, "uid", uid);
			Utilities.AddOptionalParameter(parameterList, "post_id", post_id);
			Utilities.AddOptionalParameter(parameterList, "comment", comment);

			if (isAsync)
			{
				SendRequestAsync<stream_addComment_response, string>(parameterList, uid <= 0, new FacebookCallCompleted<string>(callback), state);
				return null;
			}

			var response = SendRequest<stream_addComment_response>(parameterList, uid <= 0);
			return response == null ? null : response.TypedValue;
		}

		private bool AddLike(long uid, string post_id, bool isAsync, AddLikeCallback callback, Object state)
		{
			var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.addLike" } };
			Utilities.AddOptionalParameter(parameterList, "uid", uid);
			Utilities.AddOptionalParameter(parameterList, "post_id", post_id);

			if (isAsync)
			{
				SendRequestAsync<stream_addLike_response, bool>(parameterList, uid <= 0, new FacebookCallCompleted<bool>(callback), state);
				return true;
			}

			var response = SendRequest<stream_addLike_response>(parameterList, uid <= 0);
			return response == null ? true : response.TypedValue;
		}

        private stream_data Get(long viewer_id, List<long> source_ids, DateTime? start_time, DateTime? end_time, int? limit, string filter_key, bool isAsync, GetCallback callback, Object state)
		{
			var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.get" } };
			Utilities.AddOptionalParameter(parameterList, "viewer_id", viewer_id);
			Utilities.AddList(parameterList, "source_ids", source_ids);
			Utilities.AddOptionalParameter(parameterList, "start_time", DateHelper.ConvertDateToDouble(start_time));
			Utilities.AddOptionalParameter(parameterList, "end_time", DateHelper.ConvertDateToDouble(end_time));
			Utilities.AddOptionalParameter(parameterList, "limit", limit);
			Utilities.AddOptionalParameter(parameterList, "filter_key", filter_key);

			if (isAsync)
			{
           	    SendRequestAsync<stream_get_response>(parameterList, new FacebookCallCompleted<stream_get_response>(callback), state);
				return null;
			}

			return SendRequest<stream_get_response>(parameterList, true);
		}

		private IList<comment> GetComments(string post_id, bool isAsync, GetCommentsCallback callback, Object state)
		{
			var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.getComments" } };
			Utilities.AddRequiredParameter(parameterList, "post_id", post_id);

			if (isAsync)
			{
				SendRequestAsync<stream_getComments_response, IList<comment>>(parameterList, !string.IsNullOrEmpty(Session.SessionKey), new FacebookCallCompleted<IList<comment>>(callback), state, "comment");
				return null;
			}

            var response = SendRequest<stream_getComments_response>(parameterList, !string.IsNullOrEmpty(Session.SessionKey));
			return response == null ? null : response.comment;
		}

		private IList<stream_filter> GetFilters(long uid, bool isAsync, GetFiltersCallback callback, Object state)
		{
			var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.getFilters" } };
			Utilities.AddOptionalParameter(parameterList, "uid", uid);

			if (isAsync)
			{
				SendRequestAsync<stream_getFilters_response, IList<stream_filter>>(parameterList, uid <=0, new FacebookCallCompleted<IList<stream_filter>>(callback), state, "stream_filter");
				return null;
			}

			var response = SendRequest<stream_getFilters_response>(parameterList, uid <= 0);
			return response == null ? null : response.stream_filter;
		}

		private string Publish(string message, attachment attachment, IList<action_link> action_links, string target_id, long uid, bool isAsync, PublishCallback callback, Object state)
		{
			var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.publish" } };
			Utilities.AddOptionalParameter(parameterList, "message", message);
			if (attachment != null)
			{
				var mediaList = new List<string>();
                var prop = new Dictionary<string, string>();
                if (attachment.properties != null)
                {
                    foreach (var item in attachment.properties)
                    {
                        prop.Add(item.name, item.value.ToString());
                    }
                }   

				if (attachment.media != null)
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
				Utilities.AddOptionalParameter(parameterList, "attachment", JSONHelper.ConvertToJSONAssociativeArray(dict));
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
				Utilities.AddJSONArray(parameterList, "action_links", list);

			}
			Utilities.AddOptionalParameter(parameterList, "target_id", target_id);
			Utilities.AddOptionalParameter(parameterList, "uid", uid);

			if (isAsync)
			{
				SendRequestAsync<stream_publish_response, string>(parameterList, uid <=0, new FacebookCallCompleted<string>(callback), state);
				return null;
			}

			var response = SendRequest<stream_publish_response>(parameterList, uid <= 0);
			return response == null ? null : response.TypedValue;
		}

		private bool Remove(long uid, string post_id, bool isAsync, RemoveCallback callback, Object state)
		{
			var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.remove" } };
			Utilities.AddOptionalParameter(parameterList, "uid", uid);
			Utilities.AddRequiredParameter(parameterList, "post_id", post_id);

			if (isAsync)
			{
				SendRequestAsync<stream_remove_response, bool>(parameterList, !string.IsNullOrEmpty(Session.SessionKey), new FacebookCallCompleted<bool>(callback), state);
				return true;
			}

            var response = SendRequest<stream_remove_response>(parameterList, !string.IsNullOrEmpty(Session.SessionKey));
			return response == null ? true : response.TypedValue;
		}

		private bool RemoveComment(long uid, string comment_id, bool isAsync, RemoveCommentCallback callback, Object state)
		{
			var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.removeComment" } };
			Utilities.AddOptionalParameter(parameterList, "uid", uid);
			Utilities.AddRequiredParameter(parameterList, "comment_id", comment_id);

			if (isAsync)
			{
				SendRequestAsync<stream_removeComment_response, bool>(parameterList, uid <= 0, new FacebookCallCompleted<bool>(callback), state);
				return true;
			}

			var response = SendRequest<stream_removeComment_response>(parameterList, uid <= 0);
			return response == null ? true : response.TypedValue;
		}

		private bool RemoveLike(long uid, string post_id, bool isAsync, RemoveLikeCallback callback, Object state)
		{
			var parameterList = new Dictionary<string, string> { { "method", "facebook.stream.removeLike" } };
			Utilities.AddOptionalParameter(parameterList, "uid", uid);
			Utilities.AddRequiredParameter(parameterList, "post_id", post_id);

			if (isAsync)
			{
				SendRequestAsync<stream_removeLike_response, bool>(parameterList, uid <= 0, new FacebookCallCompleted<bool>(callback), state);
				return true;
			}

			var response = SendRequest<stream_removeLike_response>(parameterList, uid <= 0);
			return response == null ? true : response.TypedValue;
		}

		#endregion Private Methods
        
		#endregion Methods

		#region Delegates

        /// <summary>
        /// Delegate called when Get call completed
        /// </summary>
        /// <param name="data">Stream data.</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>
        /// <param name="e">Exception object, if the call resulted in exception.</param>
        //public delegate void GetBindingCallback(FacebookStreamData streamData, Object state, FacebookException e);
        public delegate void GetCallback(stream_data data, Object state, FacebookException e);
        
        /// <summary>
        /// Delegate called when GetComments call completed
        /// </summary>
        /// <param name="comments">List of comments data.</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>
        /// <param name="e">Exception object, if the call resulted in exception.</param>
        public delegate void GetCommentsCallback(IList<comment> comments, Object state, FacebookException e);

        /// <summary>
        /// Delegate called when AddComments call completed
        /// </summary>
        /// <param name="comment_id">Comment identifier.</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>
        /// <param name="e">Exception object, if the call resulted in exception.</param>
        public delegate void AddCommentCallback(string comment_id, Object state, FacebookException e);
        
        /// <summary>
        /// Delegate called when AddLike call completed
        /// </summary>
        /// <param name="result">Boolean result.</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>
        /// <param name="e">Exception object, if the call resulted in exception.</param>
        public delegate void AddLikeCallback(bool result, Object state, FacebookException e);

        /// <summary>
        /// Delegate called when GetFilters call completed
        /// </summary>
        /// <param name="filters">A List of stream_filter data.</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>
        /// <param name="e">Exception object, if the call resulted in exception.</param>
        public delegate void GetFiltersCallback(IList<stream_filter> filters, Object state, FacebookException e);

        /// <summary>
        /// Delegate called when Publish call completed
        /// </summary>
        /// <param name="post_id">Post identifer.</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>
        /// <param name="e">Exception object, if the call resulted in exception.</param>
        public delegate void PublishCallback(string post_id, Object state, FacebookException e);

        /// <summary>
        /// Delegate called when Remove call completed
        /// </summary>
        /// <param name="result">Boolean result.</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>
        /// <param name="e">Exception object, if the call resulted in exception.</param>
        public delegate void RemoveCallback(bool result, Object state, FacebookException e);

        /// <summary>
        /// Delegate called when RemoveComment call completed
        /// </summary>
        /// <param name="result">Boolean result.</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>
        /// <param name="e">Exception object, if the call resulted in exception.</param>
        public delegate void RemoveCommentCallback(bool result, Object state, FacebookException e);

        /// <summary>
        /// Delegate called when RemoveLike call completed
        /// </summary>
        /// <param name="result">Boolean result.</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>
        /// <param name="e">Exception object, if the call resulted in exception.</param>
        public delegate void RemoveLikeCallback(bool result, Object state, FacebookException e);
        
        /// <summary>
        /// Delegate called when GetActivityStream call completed
        /// </summary>
        /// <param name="result">Boolean result.</param>
        /// <param name="state">An object containing state information for this asynchronous request</param>
        /// <param name="e">Exception object, if the call resulted in exception.</param>
        public delegate void GetActivityStreamCallback(string result, Object state, FacebookException e);

		#endregion Delegates
	}

    /// <summary>
    /// helper class for calls to stream.publish with attachments
    /// </summary>
    public class attachment
    {
        /// <summary>
        /// name of attachment
        /// </summary>
        public string name { get; set; }
        /// <summary>
        /// comments_xid of attachment
        /// </summary>
        public string comments_xid { get; set; }
        /// <summary>
        /// href of attachment
        /// </summary>
        public string href { get; set; }
        /// <summary>
        /// caption of attachment
        /// </summary>
        public string caption { get; set; }
        /// <summary>
        /// description of attachment
        /// </summary>
        public string description { get; set; }
        /// <summary>
        /// list of additional properites
        /// </summary>
        public List<attachment_property> properties { get; set; }
        /// <summary>
        /// list of media
        /// </summary>
        public List<attachment_media> media { get; set; }
        /// <summary>
        /// latitude
        /// </summary>
        public string latitude { get; set; }
        /// <summary>
        /// longitude
        /// </summary>
        public string longitude { get; set; }
    }
    /// <summary>
    /// helper class the value of attachment_property
    /// </summary>
    public class attachment_property_value
    {
        /// <summary>
        /// text of the property attachment
        /// </summary>
        public String text { get; set; }
        /// <summary>
        /// href of the property attachment
        /// </summary>
        public String href { get; set; }
        /// <summary>
        /// overridden function to generate json string
        /// </summary>
        public override string ToString()
        {
            // If there is no href, then it is a simple string.
            // Otherwise, it is a JSON dictionary of text and href.
            if (String.IsNullOrEmpty(href)) return text;
            else
            {
                Dictionary<string, string> dict = new Dictionary<string, string>();
                dict.Add("text", text);
                dict.Add("href", href);
                return JSONHelper.ConvertToJSONAssociativeArray(dict);
            }
        }
    }
    /// <summary>
    /// key value pair of the attachment property
    /// </summary>
    public class attachment_property
    {
        /// <summary>
        /// key
        /// </summary>
        public String name { get; set; }
        /// <summary>
        /// value
        /// </summary>
        public attachment_property_value value { get; set; }

        /// <summary>
        /// overridden function to generate json string
        /// </summary>
        public override string ToString()
        {
            Dictionary<string, string> dict = new Dictionary<string, string>();
            dict.Add(name, value.ToString());
            return JSONHelper.ConvertToJSONAssociativeArray(dict);
        }
    }
    /// <summary>
    /// attachment_media
    /// </summary>
    public class attachment_media
	{
        /// <summary>
        /// media type
        /// </summary>
        public attachment_media_type type { get; set; }
	}

    /// <summary>
    /// sub class of attahment_media for images
    /// </summary>
    public class attachment_media_image : attachment_media
	{
        /// <summary>
        /// Constructor
        /// </summary>
        public attachment_media_image()
		{
			this.type = attachment_media_type.image;
		}
        /// <summary>
        /// image src
        /// </summary>
        public string src { get; set; }
        /// <summary>
        /// image link
        /// </summary>
        public string href { get; set; }
	}

    /// <summary>
    /// sub class of attahment_media for flash content
    /// </summary>
    public class attachment_media_flash : attachment_media
	{
        /// <summary>
        /// Constructor
        /// </summary>
        public attachment_media_flash()
		{
			this.type = attachment_media_type.flash;
		}
        /// <summary>
        /// swfsrc
        /// </summary>
        public string swfsrc { get; set; }
        /// <summary>
        /// imgsrc
        /// </summary>
        public string imgsrc { get; set; }
        /// <summary>
        /// width 
        /// </summary>
        public int width { get; set; }
        /// <summary>
        /// height 
        /// </summary>
        public int height { get; set; }
        /// <summary>
        /// expanded_width 
        /// </summary>
        public int expanded_width { get; set; }
        /// <summary>
        /// expanded_height 
        /// </summary>
        public int expanded_height { get; set; }
	}

    /// <summary>
    /// sub class of attahment_media for mp3 content
    /// </summary>
    public class attachment_media_mp3 : attachment_media
	{
        /// <summary>
        /// Constructor
        /// </summary>
        public attachment_media_mp3()
		{
			this.type = attachment_media_type.mp3;
		}
        /// <summary>
        /// src
        /// </summary>
        public string src { get; set; }
        /// <summary>
        /// title
        /// </summary>
        public string title { get; set; }
        /// <summary>
        /// artist
        /// </summary>
        public string artist { get; set; }
        /// <summary>
        /// album
        /// </summary>
        public string album { get; set; }
	}

    /// <summary>
    /// sub class of attahment_media for video content
    /// </summary>
    public class attachment_media_video : attachment_media
	{
        /// <summary>
        /// Constructor
        /// </summary>
        public attachment_media_video()
		{
			this.type = attachment_media_type.video;
		}
        /// <summary>
        /// video_src
        /// </summary>
        public string video_src { get; set; }
        /// <summary>
        /// preview_img
        /// </summary>
        public string preview_img { get; set; }
        /// <summary>
        /// video_link
        /// </summary>
        public string video_link { get; set; }
        /// <summary>
        /// video_title
        /// </summary>
        public string video_title { get; set; }
	}

    /// <summary>
    /// enum defining the types of attachments
    /// </summary>
    public enum attachment_media_type
	{
        /// <summary>
        /// image
        /// </summary>
        image,
        /// <summary>
        /// flash
        /// </summary>
        flash,
        /// <summary>
        /// mp3
        /// </summary>
        mp3,
        /// <summary>
        /// video
        /// </summary>
        video
	}
}