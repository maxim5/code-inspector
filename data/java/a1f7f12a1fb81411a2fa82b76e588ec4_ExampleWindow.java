package nu.matteus.carouselexample;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import nu.matteus.swing.carousel.BasicCarouselContainer;
import nu.matteus.swing.carousel.Carousel;
import nu.matteus.swing.carousel.CarouselListener;
import nu.matteus.swing.carousel.DefaultCarouselItem;
import nu.matteus.swing.carousel.DefaultCarouselModel;

public class ExampleWindow extends JFrame implements ActionListener, CarouselListener {
	
	/**
	 * Generated serial version UID
	 */
	private static final long serialVersionUID = 7423777186677881339L;
	private BasicCarouselContainer carouselContainer;
	private Carousel carousel;
	private DefaultCarouselModel carouselModel;
	private JPanel contentPane;
	
	public ExampleWindow() throws Exception {
		
		// Setup content pane
		contentPane = (JPanel)this.getContentPane();
		contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
		contentPane.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

		// Carousel
		carousel = new Carousel();
		carousel.addCarouselListener(this);
		carousel.setNumberOfVisibleItems(4);
		carouselModel = (DefaultCarouselModel)carousel.getModel();
		populateCarousel();
		
		carouselContainer = new BasicCarouselContainer();
		carouselContainer.setCarousel(carousel);
		contentPane.add(carouselContainer);
		
		// Carousel 2 (using the same model as the first one)
		Carousel carousel2 = new Carousel();
		carousel2.addCarouselListener(this);
		carousel2.setNumberOfVisibleItems(2);
		carousel2.setModel(carouselModel);
		contentPane.add(new BasicCarouselContainer(carousel2));
		
		this.setLocationByPlatform(true);
		this.setTitle("Example Carousel");
		this.setDefaultCloseOperation(EXIT_ON_CLOSE);
		// this.pack();
		this.setSize(700, 540);
		
		this.setVisible(true);
	}
	
	protected void populateCarousel() {
		
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

	@Override
	public void actionPerformed(ActionEvent e) {
		switch(e.getActionCommand()) {
		case "Next":
			carousel.Next();
			break;
		case "Previous":
			carousel.Previous();
			break;
		}
	}

	@Override
	public void onCarouselPageChanged(Carousel sender, int newIndex) {
		
	}

	@Override
	public void onCarouselItemClicked(Carousel carousel, Object o) {
		JOptionPane.showMessageDialog(this, "You clicked on " + o);
	}
	
}
