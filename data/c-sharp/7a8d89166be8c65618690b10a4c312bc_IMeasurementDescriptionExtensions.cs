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
using ProbeNet.Messages.Interface;
#if MICRO_FRAMEWORK
using System.Collections;
#else
using System.Collections.Generic;
#endif

namespace ProbeNet.Messages
{
    /// <summary>
    /// Extensions for the <see cref="IMeasurementDescription"/> interface.
    /// </summary>
    public static class IMeasurementDescriptionExtensions
    {
        /// <summary>
        /// Finds the measurement description by identifier.
        /// </summary>
        /// <returns>The the measurement description.</returns>
        /// <param name="resultDescriptions">List of measurement description.</param>
        /// <param name="id">Identifier.</param>

#if MICRO_FRAMEWORK
        public static IMeasurementDescription FindMeasurementDescriptionById(this IList resultDescriptions,
#else
        public static IMeasurementDescription FindById(this IList<IMeasurementDescription> resultDescriptions,
#endif
 string id)
        {
            foreach (IMeasurementDescription measurementDescription in resultDescriptions) {
                if (measurementDescription.Id == id) {
                    return measurementDescription;
                }
            }
            return null;
        }
    }
}

