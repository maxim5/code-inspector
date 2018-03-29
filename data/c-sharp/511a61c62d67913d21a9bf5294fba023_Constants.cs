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

namespace ProbeNet.Messages
{
    /// <summary>
    /// Constants.
    /// </summary>
	public static class Constants
	{
        /// <summary>
        /// The version of the ProbeNet Protocoll
        /// </summary>
		public const int ProbeNetVersion = 0;
        /// <summary>
        /// The default language.
        /// </summary>
		public const string DefaultLanguage = "en";

        /// <summary>
        /// The type of the discovery answer message.
        /// </summary>
		public const string DiscoveryAnswerMessageType = "iSpeakProbeNet";
        /// <summary>
        /// The type of the discovery request message.
        /// </summary>
		public const string DiscoveryRequestMessageType = "whoSpeaksProbeNet";

        /// <summary>
        /// The i18n resource prefix.
        /// </summary>
        public const string I18nResourcePrefix = "ProbeNet.Messages.i18n";
        /// <summary>
        /// The probe net messages domain.
        /// </summary>
        public const string ProbeNetMessagesDomain = "probenet-messages";

        /// <summary>
        /// The maximum length of the short caption.
        /// </summary>
        public const int MaximumShortCaptionLength = 10;
	}
}
