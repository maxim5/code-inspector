/*
 * Data container for saving a Cellular automaton. Along the state of the actual Cellular Automaton 
 * we want to save it's history along other data of interest.
 * Author: Anders Hřst | ahkj@itu.dk
 * Date: 12 dec 2011
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using AutomaTones.Model.Automaton_Model;
using Midi;

namespace AutomaTones.Model.Tools {
    [Serializable]
    public class CaSaveContainer {
        public CellularAutomaton CellularAutomaton { get; set; }
        public List<CellularAutomaton> CaHistory { get; set; }
        public Channel ChosenChannel { get; set; }
        public int Volume { get; set; }
        public Size Grid { get; set; }
    }
}
