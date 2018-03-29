# -*- coding: utf-8 -*-
"""
/***************************************************************************
 MapColoringDialog
                                 A QGIS plugin
 Coloring a map with minimal number of color
                             -------------------
        begin                : 2012-09-24
        copyright            : (C) 2012-2015 by Alain Delplanque
        email                : alaindelplanque@laposte.net
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
"""

from monitor import Monitor


class Algorithm(Monitor):

    def __init__(self, m, progress_cb=None):
        """
        Constructor
        @param m Graph
        @param progress_cb Callback to inform of algorithme progress
        """
        super(Algorithm, self).__init__(progress_cb)
        self.m = m
        self._vertexColor = self.run()

    def vertexColor(self):
        return self._vertexColor
