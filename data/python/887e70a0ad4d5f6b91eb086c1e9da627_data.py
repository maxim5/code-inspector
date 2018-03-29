"""
data for the domain_drawing tool

Primarily colour maps at the moment.
"""

"""
ubl_col_map = {
    "E1": "#000000"
    "HECT": "#903599", # purple
    "RNF domain": "#971010", #reds #nt # Not in use?
    "BTB": "#b45353",
    "SOCS box": "#fd3e3e", #nt
    "F box": "#fe7979", #nt
    "U box": "orange", # orange  #nt
    "ZnF A20": "#385F72", #blues #nt
    "DDB1-like": "#2D7BA0", #nt
    "Mixed-class": "#6EB7DA", #nt
    "E2": "#32CA32", # green #nt
    "RING finger": "#FEA9A9"
    }
"""

ubl_col_map = {
    "E1": "#00FF00",
    "E2": "#009400",
    "HECT": "#00C7FF", # E3s
    #"RING finger": "#FF8100",
    "RNF finger": "#FF8100",
    "BTB": "#B37536",
    "SOCS box": "#944B00",
    "F box": "#FFA64B",
    "U box": "#FFFC00",
    "ZnF A20": "#9C00FF",
    "DDB1-like": "#1F00FF",
    "UCH": "#FF0000", # DUBs
    "USP": "#B33636",
    "MJD": "#940000",
    "OTU": "#FF4B4B",
    "JAMM": "#FF8686",
    "Cullin": "#888888", # Structural-like?
    "VHL": "#444444"
    }
  
domain_label_lookup = { # Needs to match the name in the ubl_col_map, these are for mixed domains
    "PF10585": "E1",
    "PF00179": "E2",
    "SM00212": "E2",
    "SSF54495": "E2", # Missing from original data
    "PF01754": "ZnF A20",
    "SM00259": "ZnF A20",
    "PF04564": "U box",
    "SM00504": "U box",
    "PF00646": "F box",
    "SSF81383": "F box",
    "SM00256": "F box",
    "PF07525": "SOCS box",
    "SM00253": "SOCS box",
    "SM00969": "SOCS box",
    "PF00097": "RNF finger",
    "PF02207": "RNF finger",
    "SM00184": "RNF finger",
    "SSF57850": "RNF finger",
    "SM00396": "RNF finger",
    "PF00651": "BTB",
    "SM00225": "BTB",
    "SSF54695": "BTB",
    "PF00632": "HECT",
    "SM00119": "HECT",
    "SSF56204": "HECT",
    "PF03178": "DDB1-like",
    "PF01088": "UCH",
    "PF00443": "USP",
    "PF02099": "MJD",
    "PF02338": "OTU",
    "SM00232": "JAMM",
    "PF01398": "JAMM",
    "PF00888": "Cullin",
    "PF01847": "VHL",
    "SSF49468": "VHL"
    }  
  
acast_col_map = {"SUPERFAMILY": "grey",
    "Pfam-A": "blue", 
    "SMART": "red"
    }

unk_domains = {
    # metabolic
    "Decarboxylase2": "#FF0000",                      
    "Caspase-like": "#BF3030", 
    "Hydrolase_Ploop": "#A60000",
    "ProtInh_I27": "#FF5858", # Calpain inhibitor
    # Transcriptional
    "Krueppel": "#FFC300",                           
    "TDP": "#BF9E30", # E2F
    "TF_DP": "#A67F00", # E2F
    "HMG_family": "#FFD858", 
    # Regulatory
    "Mov34": "#6400FF",  # proteasomal, eIF3,              
    "RNApol_subunit": "#6830BF", 
    "HATdim": "#4100A6", 
    "GBP": "#9A58FF", # IFng induced GTPase
    # Structural...
    "GagP30": "#00A0FF", # Viral assembly protein! WTF?!   
    "ChlathrinAdaptor": "#0068A6", # Golgi protein
    "SteadinessBox": "#9FDBFF", # endosome/Lysosome sorter
    # ZnFs. 
    "ZnF_bba": "#cccccc",                               
    "ZnF_retrovirus": "#999999", 
    "ZnF_CCHC": "#666666", 
    "ZnF_C2H2": "#333333", 
    # Ribosomal
    "RibProt_L14b": "#00FF00",                           
    "RibProt_S24e": "#30BF30",
    "RibProt_S4e": "#00A600",
    # Immunoglobulin C-type
    "Imm_C1set": "#FF8700",                                
    "Ig": "#FFD29F", 
    # Unknowns
    "DUF1725": "black",                               
    "DUF1171": "black", 
    "DUF3704": "black", 
     # Ub-like
    "Ubiquitin-like": "#FF00B2",                       
    "UbiqConj_RWD": "#A60074", 
    "L1": "#FF9FE2"} 
    
ptp_map = {
	'EYA': '#a6cee3',
	'SSF52788': '#2078b4',
	'SSF52799': '#afdd8a',
	'Myotub-related': '#35a12e',
	'Tyr_Pase_low_mol_wt': '#fa9897',
	'Tyr_Pase_rcpt/non-rcpt': '#e31a1c',
	'MPI_Phosphatase': '#fcbe6e',
	'DUF370': '#fe7f01',
	'Tyr_Pase_cat': '#c7afd4',
	'Tyr_Pase_dual_specific': '#6c4099',
	'Tyr_Pase_SIW14-like': '#fefd98',
	}
	
"""
my @E2=('PF00179','SM00212');
my @ZnF_A20= ('PF01754','SM00259' );
my @Ubox= ('PF04564','SM00504');
my @Fbox = ('PF00646','SSF81383','SM00256');
my @SOCSbox =('PF07525','SM00253','SM00969');
my @RING=('PF00097','PF02207','SM00184','SSF57850','SM00396');
my @BTB=('PF00651','SM00225','SSF54695');
my @HECT=('PF00632','SM00119','SSF56204');
my @DDB1=('PF03178')
"""

def draw_key(name, colmap, order=None):
    """
    Draw the key for the current colmap
    """
    fig = plot.figure()
    ax = fig.add_subplot(111)
    ax.set_xlim([0, 20])
    ax.set_ylim([-len(colmap), 2])
    
    if not order: 
        order = colmap
    
    for i, k in enumerate(order):
        ax.add_patch(Rectangle((2, -i), 6.8, 0.8, 
                        ec="none", fc=colmap[k], lw=0.5, zorder=2))
        ax.text(10, -(i-0.4), str(k), ha="left", va="center", fontsize=12, color="black")
    fig.savefig("%s_key.png" % name)
    return(None)
    
if __name__ == "__main__":
    import matplotlib.pylab as plot
    from matplotlib.patches import Rectangle
    ubl_order = ["E1", "E2", "HECT", "RNF finger", "BTB", "SOCS box", "F box",
        "U box", "ZnF A20", "DDB1-like", "UCH", "USP", "MJD", "OTU", "JAMM",
        "Cullin", "VHL"]
    draw_key("ubl", ubl_col_map, ubl_order)
    draw_key("ptp", ptp_map, ptp_map.keys())