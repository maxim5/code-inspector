package nu.matteus.dragndropexample;

import java.awt.BorderLayout;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDropEvent;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.TransferHandler;

import nu.matteus.swing.carousel.BasicCarouselContainer;
import nu.matteus.swing.carousel.Carousel;
import nu.matteus.swing.carousel.CarouselListener;
import nu.matteus.swing.carousel.DefaultCarouselItem;
import nu.matteus.swing.carousel.DefaultCarouselModel;
import nu.matteus.swing.shoppingcart.ShoppingCart;
import nu.matteus.swing.shoppingcart.ShoppingCartItem;

public class DragDropExampleWindow extends JFrame {
	
	/**
	 * Generated serial version UID
	 */
	private static final long serialVersionUID = 7423777186677881339L;
	
	/**
	 * Container of the window.
	 */
	private JPanel contentPane;
	
	/**
	 * Carousel component with some movie titles.
	 */
	private Carousel carousel;
	
	/**
	 * Model of the <code>Carousel</code>. 
	 */
	private DefaultCarouselModel carouselModel;
	
	/**
	 * Container of the <code>Carousel</code>.
	 */
	private BasicCarouselContainer carouselContainer;
	
	/**
	 * Shopping cart that can hold items from the carousel.
	 */
	private ShoppingCart shoppingCart;
	
	/**
	 * List 1
	 */
	JList<String> list1;
	
	/**
	 * List 2
	 */
	JList<String> list2;
	
	/**
	 * Creates a new instance of a <code>DragDropExampleWindow</code>.
	 */
	public DragDropExampleWindow() {
		
		// Setup panes
		contentPane = (JPanel)this.getContentPane();
		contentPane.setLayout(new BorderLayout());
		
		JPanel mainPane = new JPanel();
		mainPane.setLayout(new BoxLayout(mainPane, BoxLayout.Y_AXIS));
		mainPane.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		contentPane.add(mainPane, BorderLayout.CENTER);
		
		JPanel rightPane = new JPanel();
		rightPane.setLayout(new BoxLayout(rightPane, BoxLayout.Y_AXIS));
		rightPane.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		contentPane.add(rightPane, BorderLayout.EAST);
		
		// Initialize components
		initializeCarousel();
		initializeShoppingCart();
		initializeLists();
		
		// Add components to main pane
		mainPane.add(carouselContainer);
		mainPane.add(shoppingCart);
		
		// Add lists to right pane
		rightPane.add(list1);
		rightPane.add(list2);
		
		this.setLocationByPlatform(true);
		this.setTitle("Drag n Drop Example");
		this.setDefaultCloseOperation(EXIT_ON_CLOSE);
		this.pack();
		
		this.setVisible(true);
	}

	private void initializeLists() {
		
		// Create list models
		DefaultListModel<String> listModel1 = new DefaultListModel<String>();
		listModel1.addElement("AAAA");
		listModel1.addElement("BBBB");
		listModel1.addElement("CCCC");
		listModel1.addElement("DDDD");
		
		DefaultListModel<String> listModel2 = new DefaultListModel<String>();
		listModel2.addElement("EEEE");
		listModel2.addElement("FFFF");
		listModel2.addElement("GGGG");
		listModel2.addElement("HHHH");
		
		// Create lists
		list1 = new JList<>(listModel1);
		list1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		
		list2 = new JList<>(listModel2);
		list2.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		
		// Drag and Drop support for lists
		TransferHandler transferHandler = new TransferHandler() {
			private static final long serialVersionUID = 6148147923742967846L;
			
			@Override
			public int getSourceActions(JComponent c) {
				return MOVE;
			}
			@Override
			protected Transferable createTransferable(JComponent c) {
				return new StringSelection((String) ((JList<?>)c).getSelectedValue());
			}
			@Override
			protected void exportDone(JComponent source, Transferable data,
					int action) {
				if(action == MOVE) {
					JList<?> list = (JList<?>)source;
					DefaultListModel<?> model = (DefaultListModel<?>)list.getModel();
					Object o;
					try {
						o = data.getTransferData(DataFlavor.stringFlavor);
					} catch (UnsupportedFlavorException | IOException e) {
						e.printStackTrace();
						return;
					}
					model.removeElement(o);
				}
			}
			@Override
			public boolean canImport(JComponent comp,
					DataFlavor[] transferFlavors) {
				for(DataFlavor flavor : transferFlavors) {
					if(flavor.isFlavorTextType()) {
						return true;
					}
				}
				return false;
			}
			@SuppressWarnings("unchecked")
			@Override
			public boolean importData(JComponent comp, Transferable t) {
				try {
					String data = (String)t.getTransferData(DataFlavor.stringFlavor);
					JList<String> list = (JList<String>)comp;
					DefaultListModel<String> model = (DefaultListModel<String>)list.getModel();
					model.addElement(data);
					return true;
				} catch (UnsupportedFlavorException | IOException e) {
					e.printStackTrace();
					return false;
				}
				
			}
		};
		
		
		list1.setTransferHandler(transferHandler);
		list1.setDragEnabled(true);
		
		list2.setTransferHandler(transferHandler);
		list2.setDragEnabled(true);
	}

	/**
	 * Initializs the shopping cart and makes it a drop target,
	 * so that it can recieve items from the carousel.
	 */
	private void initializeShoppingCart() {
		shoppingCart = new ShoppingCart();
		shoppingCart.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		shoppingCart.setDropTarget(new DropTarget() {

			/**
			 * Generated serial version UID
			 */
			private static final long serialVersionUID = -3556088026718217819L;

			@Override
			public synchronized void drop(DropTargetDropEvent dtde) {
				
				DataFlavor flavor = new DataFlavor(Object.class, "Object");
				
				if(dtde.isDataFlavorSupported(flavor)) {
					try {
						Object o = dtde.getTransferable().getTransferData(flavor);
						if(o instanceof DefaultCarouselItem) {
							DefaultCarouselItem item = (DefaultCarouselItem)o;
							shoppingCart.getModel().add(new ShoppingCartItem(
								item.getTitle(),
								1, 10
							));
						}
					} catch (UnsupportedFlavorException e) {
						e.printStackTrace();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
				
				super.drop(dtde);
			}
		});
	}
	
	/**
	 * Initializes the carousel and its container.
	 */
	private void initializeCarousel() {
		carousel = new Carousel();
		carouselModel = (DefaultCarouselModel)carousel.getModel();
		carouselContainer = new BasicCarouselContainer(carousel);
		carouselContainer.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		populateCarousel();
		addCarouselListener();
	}
	
	/**
	 * Adds a anonymous <code>CarouselListener</code> to the <code>Carousel</code>
	 * that adds items to the shopping cart on item click events.
	 */
	private void addCarouselListener() {
		carousel.addCarouselListener(new CarouselListener() {
			
			@Override
			public void onCarouselPageChanged(Carousel sender, int newIndex) {
				
			}
			
			@Override
			public void onCarouselItemClicked(Carousel carousel, Object o) {
				shoppingCart.getModel().add(new ShoppingCartItem(
					((DefaultCarouselItem)o).getTitle(),
					1, 99
				));
			}
		});
	}

	/**
	 * Populates the carousel with movie titles.
	 */
	private void populateCarousel() {
		
		try {
			carouselModel.Add(new DefaultCarouselItem(
					"Looper",
					"Looper är en film.",
					"/nu/matteus/carouselexample/images/looper.jpg"
			));
			
			carouselModel.Add(new DefaultCarouselItem(
					"Resident Evil: Retribution",
					"Text.",
					"/nu/matteus/carouselexample/images/resident-evil-retribution.jpg"
			));
			
			carouselModel.Add(new DefaultCarouselItem(
					"Skyfall",
					"Text.",
					"/nu/matteus/carouselexample/images/skyfall.jpg"
			));
			
			carouselModel.Add(new DefaultCarouselItem(
					"Taken 2",
					"Text.",
					"/nu/matteus/carouselexample/images/taken-2.jpg"
			));
			
			carouselModel.Add(new DefaultCarouselItem(
					"The Amazing Spiderman",
					"Text.",
					"/nu/matteus/carouselexample/images/the-amazing-spider-man.jpg"
			));
			
			carouselModel.Add(new DefaultCarouselItem(
					"The Bourne Legacy",
					"Text.",
					"/nu/matteus/carouselexample/images/the-bourne-legacy.jpg"
			));
			
			carouselModel.Add(new DefaultCarouselItem(
					"The Dark Knight Rises",
					"Text.",
					"/nu/matteus/carouselexample/images/the-dark-knight-rises.jpg"
			));
			
			carouselModel.Add(new DefaultCarouselItem(
					"The Hobbit: An unexpected Journey",
					"Text.",
					"/nu/matteus/carouselexample/images/the-hobbit-an-unexpected-journey.jpg"
			));
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
}
