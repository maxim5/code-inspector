/*
 * Data types for the ProbeNet Protocol in C#
 * Copyright (C) Wolfgang Silbermayr
 * Copyright (C) Florian Marchl
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
using System;
using System.Collections;
#if MICRO_FRAMEWORK

#else
using Newtonsoft.Json;
#endif

namespace ProbeNet.Messages
{
    /// <summary>
    /// Message header.
    /// </summary>
#if !MICRO_FRAMEWORK
    [JsonObject(MemberSerialization.OptIn)]
#endif
    public class MessageHeader
    {
        private string language;
        private DateTime moment;
        private string tag;

        /// <summary>
        /// Initializes a new instance of the <see cref="ProbeNet.Messages.MessageHeader"/> class.
        /// </summary>
        public MessageHeader() :
            this(Constants.DefaultLanguage, DateTime.Now, null)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ProbeNet.Messages.MessageHeader"/> class.
        /// </summary>
        /// <param name="language">Language.</param>
        public MessageHeader(string language) :
            this(language, DateTime.Now, null)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ProbeNet.Messages.MessageHeader"/> class.
        /// </summary>
        /// <param name="language">Language.</param>
        /// <param name="moment">Moment.</param>
        /// <param name="tag">Tag.</param>
        public MessageHeader(string language, DateTime moment, string tag)
        {
            this.language = language;
            this.moment = moment;
            this.tag = tag;
        }

#if MICRO_FRAMEWORK
        public MessageHeader(Hashtable deserialized)
        {
            Language = (string)deserialized["language"];
            Moment = DateTime.Now;//TODO:  Moment = (DateTime)deserialized["moment"];
            Tag = (string)deserialized["tag"];
        }
#endif

        /// <inheritdoc/>
#if !MICRO_FRAMEWORK
        [JsonProperty(LanguageString)]
#endif
        public string Language
        {
            get
            {
                return this.language;
            }
            set
            {
                language = value;
            }
        }

        /// <inheritdoc/>
#if !MICRO_FRAMEWORK
        [JsonProperty(MomentString)]
        [JsonConverter(typeof(ProbeNetDateTimeConverter))]
#endif
        public DateTime Moment
        {
            get
            {
                return this.moment;
            }
            set
            {
                moment = value;
            }
        }

        /// <inheritdoc/>
#if !MICRO_FRAMEWORK
        [JsonProperty(TagString)]
#endif
        public string Tag
        {
            get
            {
                return this.tag;
            }
            set
            {
                tag = value;
            }
        }

        private const string LanguageString = "language";
        private const string MomentString = "moment";
        private const string TagString = "tag";

        public override string ToString()
        {
            return "{\"" +
                LanguageString + "\":\"" + Language + "\",\"" +
                MomentString + "\":\"" + Moment + "\",\"" +
                TagString + "\":\"" + Tag +
                "\"}";
        }
    }
}

