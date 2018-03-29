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

#if MICRO_FRAMEWORK
using System.Collections;
#else
using Newtonsoft.Json;
#endif

namespace ProbeNet.Messages
{
    /// <summary>
    /// Message.
    /// </summary>
#if !MICRO_FRAMEWORK
    [JsonObject(MemberSerialization.OptIn)]
#endif
    public class Message
    {
        private MessageHeader header;
        private IMessageBody body;

        /// <summary>
        /// Initializes a new instance of the <see cref="ProbeNet.Messages.Message"/> class.
        /// </summary>
        /// <param name="header">Header.</param>
        /// <param name="body">Body.</param>
        public Message(MessageHeader header, IMessageBody body)
        {
            this.header = header;
            this.body = body;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ProbeNet.Messages.Message"/> class with a default header.
        /// </summary>
        /// <param name="body">Body.</param>
        public Message(IMessageBody body) :
            this(new MessageHeader(), body)
        {
        }

#if MICRO_FRAMEWORK
        /// <summary>
        /// Initialize a new instance and set values according to specified hashtable
        /// </summary>
        /// <param name="deserialized">The deserialized values.</param>
        public Message(Hashtable deserialized)
        {
            Header = new MessageHeader((Hashtable)deserialized["header"]);
            Body = new MessageBody((Hashtable)deserialized["body"]);
        }
#endif

        /// <summary>
        /// Gets or sets the header.
        /// </summary>
        /// <value>The header.</value>
#if !MICRO_FRAMEWORK
        [JsonProperty(HeaderString)]
#endif
        public MessageHeader Header
        {
            get
            {
                return header;
            }
            set
            {
                header = value;
            }
        }

        /// <summary>
        /// Gets or sets the body.
        /// </summary>
        /// <value>The body.</value>
#if !MICRO_FRAMEWORK
        [JsonProperty(BodyString)]
#endif
        public IMessageBody Body
        {
            get
            {
                return this.body;
            }
            set
            {
                body = value;
            }
        }

        private const string HeaderString = "header";
        private const string BodyString = "body";

        public override string ToString()
        {
#if MICRO_FRAMEWORK
            //return Json.NETMF.JsonSerializer.SerializeObject(this);
            return "{\"" +
                HeaderString + "\":" + Header + ",\"" +
                BodyString + "\":" + Body +
                "}";
#else
            return base.ToString();
#endif
        }
    }
}

