//
//  DataController.m
//  Nellodee
//
//  Created by Ada Hopper on 09/08/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "CategoriesDataController.h"
#import "Category.h"

@implementation CategoriesDataController

@synthesize categoriesList;

- (id)init {
    if (self = [super init]) {
        [self loadData];
    }
    return self;
}

- (void)loadData {
    
    
    NSMutableArray *categories = [[NSMutableArray alloc] init];
	Category *cat;
	
	cat = [[Category alloc]init];
	cat.title=@"Medicine and Dentistry";
	cat.tag=@"medicineanddentistry";
	cat.subcategories = [[NSDictionary alloc] 
						 initWithObjects:[[NSArray alloc] 
										  initWithObjects:@"Pre-clinical Medicine",@"Pre-clinical Dentistry",@"Clinical Medicine",
														  @"Clinical Dentistry",@"Others in Medicine and Dentistry",nil] 
						 forKeys:[[NSArray alloc] 
								  initWithObjects:@"preclinicalmedicine",@"preclinicaldentistry",@"clinicalmedicine",
								  @"clinicaldentistry",@"othersinmedicineanddentistry",nil]];
   	[categories addObject:cat];
	[cat release];

	cat = [[Category alloc]init];
	cat.title=@"Biological Sciences";
	cat.tag=@"biologicalsciences";
	cat.subcategories = [[NSDictionary alloc] 
						 initWithObjects:[[NSArray alloc]
										  initWithObjects:@"Biology",@"Botany",@"Zoology", @"Genetics",@"Microbiology",
										  @"Sports Science", @"Molecular Biology, Biophysics and Biochemistry",
										  @"Psychology",@"Others in Biological Sciences",nil]
						 forKeys:[[NSArray alloc]
								  initWithObjects:@"biology",@"botany",@"zoology", @"genetics",@"microbiology",
										@"sportsscience", @"molecularbiologybiophysicsandbiochemistry",
										@"psychology",@"othersinbiologicalsciences",nil]];
   	[categories addObject:cat];
	[cat release];

	cat = [[Category alloc]init];
	cat.title=@"Veterinary Sciences and Agriculture";
	cat.tag=@"veterinarysciencesagriculture";
	cat.subcategories = [[NSDictionary alloc] 
						 initWithObjects:[[NSArray alloc] 
										  initWithObjects:@"Pre-clinical Veterinary Medicine",
										  @"Clinical Veterinary Medicine and Dentistry", @"Animal Science", 
										  @"Agriculture",@"Forestry",@"Food and Beverage studies"
										  @"Agricultural Sciences",@"Others in Veterinary Sciences and Agriculture",nil]
						 forKeys:[[NSArray alloc]
								  initWithObjects:@"preclinicalveterinarymedicine",
								  @"clinicalveterinarymedicineanddentistry", @"animalscience", 
								  @"agriculture",@"forestry",@"foodandbeveragestudies"
								  @"agriculturalsciences",@"others",nil]];
   	[categories addObject:cat];
	[cat release];

	cat = [[Category alloc]init];
	cat.title=@"Physical Sciences";
	cat.tag=@"physicalsciences";						 
	cat.subcategories = [[NSDictionary alloc] 
					  initWithObjects:[[NSArray alloc] 
									   initWithObjects:@"Chemistry",@"Materials Science",@"Physics", 
									   @"Forensic and Archaeological Science",@"Astronomy",@"Geology", 
									   @"Ocean Sciences",@"Others in Physical Sciences",nil]
					  forKeys:[[NSArray alloc]
							   initWithObjects:@"chemistry",@"materialsscience",@"physics",@"forensicandarchaeologicalscience",
							   @"astronomy",@"geology",@"oceansciences",@"others",nil]];
				  
	[categories addObject:cat];
	[cat release];

	cat = [[Category alloc]init];
	cat.title=@"Mathematical and Computer Sciences";
	cat.tag=@"mathematicalandcomputersciences";
	cat.subcategories = [[NSDictionary alloc] initWithObjects:[[NSArray alloc] 
															   initWithObjects:@"Mathematics",@"Operational Research",@"Statistics", 
															   @"Computer Science", @"Information Systems",@"Software Engineering", 
															   @"Artificial Intelligence", @"Others in Mathematical and Computing Sciences",nil] 
													  forKeys:[[NSArray alloc]
															   initWithObjects:@"mathematics",@"operationalresearch",@"statistics",@"computerscience",
															   @"informationsystems",@"softwareengineering",@"artificialintelligence",@"others",nil]];
	


	[categories addObject:cat];
	[cat release];
	// ...

	self.categoriesList = categories;
	[categories release];
}

- (NSString *) categoriesShoWithTag:(NSString*)tag{
	NSString *category = [[NSString alloc] initWithString:@""];
	NSArray *catSubcat = [[[NSArray alloc] init] autorelease];
	catSubcat = [tag componentsSeparatedByString:@"/"];
	
	for (Category *cat in self.categoriesList){
		//NSLog(@"Tag : %@",[cat tag]);
		if([[catSubcat objectAtIndex:1] isEqualToString:[cat tag]]){
			category = [category stringByAppendingString:[cat title]];
			//NSLog(@"Category : %@",category);
			if([catSubcat count]>2){
				category = [category stringByAppendingString:@" > "];
				category = [category stringByAppendingString:[[cat subcategories] objectForKey:[catSubcat objectAtIndex:2]]];  
			}
			break;
		}
	
	}
	
	return category;

}

-(void) dealloc{
	[categoriesList release];
	[super dealloc];
}
@end
