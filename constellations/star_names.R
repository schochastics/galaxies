#https://www.fantasynamegenerators.com/constellation-names.php


get_starname <- function(n){
  nm1 <- c("Acanthuridae","Achatina","Achatinoidea","Acidalia","Acinonyx","Actinidia",
           "Aedes","Aegypius","Aepyceros","Ailuropoda","Ailurus","Aitheria","Ajaja",
           "Alcelaphinae","Alces","Alligator","Allita","Alopex","Alouatta","Ambystoma",
           "Amphiprioninae","Anas","Anguis","Anisoptera","Anthozoa","Antilocapra","Apatura",
           "Apis","Apodemus","Apolline","Apollonia","Aptenodytes","Arachnocampa","Arctictis",
           "Arctocephalinae","Ardeidae","Arini","Arvicola","Asta","Asteria","Asterodea","Astra",
           "Astrea","Astrochelys","Atelerix","Balaenoptera","Balsenoptera","Barbus","Betta",
           "Bison","Blattaria","Bombina","Bombus","Bos","Brachypelma","Brachyura","Branta",
           "Bubalus","Bubo","Bufo","Buteo","Cacajao","Caelestra","Caeli","Caelia","Caelifera",
           "Caimaninae","Callithrix","Calva","Camelus","Canis","Canus","Capra","Caracal","Carcharhinus",
           "Carcharodon","Caridea","Castor","Casuarius","Caudata","Cavia","Cebus","Celaeno","Celesse",
           "Celesta","Celeste","Celestia","Celestiel","Celestina","Celestine","Celestyna","Celia",
           "Celine","Cephalopterus","Ceratophrys","Ceratotherium","Cerura","Cervus","Cetorhinus",
           "Cettia","Chelonioidea","Chelydridae","Chilopoda","Chinchilla","Chiroptera",
           "Chlamyphorus","Chlorocebus","Choeropsis","Choloepus","Cichlidae","Cirripedia",
           "Civettictis","Clethrionomys","Cnidaria","Coccinellidae","Coleoptera","Connochaetes",
           "Coraciiformes","Coturnix","Crocodylus","Crocuta","Cryptoprocta","Cuon","Cygnus","Damia",
           "Dasyatis","Dasypodidae","Dasyurus","Daubentonia","Delphinus","Demospongiae","Dendrobatidae",
           "Dendrobranchiata","Dermaptera","Desmodontinae","Dicerorhinus","Diceros","Didelphis","Diomedeidae",
           "Diplopoda","Dipodomys","Diptera","Dracaena","Dromaius","Dugong","Dynastes","Echinoidea","Eleadora",
           "Eleana","Electrophorus","Elephantulus","Elephas","Eliana","Elianna","Elianne","Emydidae","Enhydra",
           "Ephemeroptera","Eptesicus","Erethizon","Erithacus","Erythrocebus","Esox","Essie","Estella","Estelle",
           "Eudyptes","Eudyptula","Eustella","Eutamias","Falconiforme","Fanum","Felis","Formicidae","Fratercula",
           "Fregata","Funambulus","Galeocerdo","Gallinula","Gallus","Gavia","Gavialis","Gekkonidae","Geochelone",
           "Gerbillinae","Gerridae","Ginglymostoma","Giraffa","Glaucomys","Gliridae","Gopherus","Gorilla","Gruidae",
           "Gulo","Gynnidomorpha","Halichoerus","Helarctos","Heleioporus","Helia","Heloderma","Helogale","Hemigalus",
           "Hester","Heterodontus","Hieraatus","Hippopotamus","Holothuroidea","Hydrochoerus","Hydrodamalis","Hydrurga",
           "Hyla","Hylobatidae","Hymenoptera","Idalia","Iguana","Indri","Iris","Irisa","Isoptera","Labridae",
           "Lacerta","Lacertilia","Lagenorhynchus","Lagothrix","Lama","Larva","Latrodectus","Lemmus","Lemur",
           "Leontopithecus","Leopardus","Lepisosteidae","Leptailurus","Lepus","Limulidae","Lissotriton",
           "Litoria","Lontra","Lopholithodes","Loxodonta","Lucanidae","Luscinia","Lutra","Lycaon",
           "Lynx","Macaca","Macropus","Maia","Malleus","Mammuthus","Mandrillus","Manta","Marmota",
           "Martes","Megadyptes","Megaptera","Meleagris","Melopsittacus","Mephitis","Merops","Mesobatrachia",
           "Mesocricetus","Metynnis","Microcebus","Microsorex","Microtus","Mirounga","Moloch","Muraenidae",
           "Mustela","Myotis","Myrmecobius","Myrmecophaga","Nandinia","Nasalis","Nasua","Nectophryne","Neofelis",
           "Neotoma","Nephropidae","Numididae","Nyctereutes","Ochotona","Octopus","Odobenus","Odocoileus","Okapia",
           "Ondatra","Oniscidea","Ophisaurus","Orcinus","Oreamnos","Oriana","Oriolus","Ornithorhynchus","Oryctolagus",
           "Osteolaemus","Ostreidae","Otariidae","Ovis","Paguma","Paguroidea","Panthera","Papilionoidea","Papio",
           "Paracheirodon","Paradoxurus","Paralichthys","Passeridae","Pavo","Pecari","Pelecanus","Pelophylax","Perameles",
           "Peromyscus","Phacochoerus","Phaethon","Phalanger","Phasianus","Phasmatodea","Phoca","Phoenicopterus","Phycodurus",
           "Physeter","Physignathus","Pica","Picidae","Platanistoidea","Pleione","Poecilia","Pogona","Pomacanthidae","Pongo",
           "Prionailurus","Pristella","Procavia","Procyon","Proteus","Protoreaster","Pseudoryx","Psittacine","Pterois",
           "Pteromyini","Pygocentrus","Pygoscelis","Ramphastos","Rana","Rangifer","Raphus","Rattus",
           "Recurvirostra","Rhincodon","Rhinoceros","Rhinocerotidae","Rhinoderma","Rupicapra","Saguinus",
           "Saimiri","Sarcophilus","Sciuridae","Scorpaenidae","Scorpiones","Selena","Selene","Selenia","Sepiida",
           "Serpentes","Setonix","Sidra","Siluriformes","Simia","Smilodon","Sorex","Spermophilus","Spheniscus",
           "Sphenodon","Sphyraena","Sphyrna","Squalus","Star","Stegostoma","Stella","Stelle","Strigops","Strix",
           "Struthio","Sula","Suricata","Sylvilagus","Symphysodon","Syncerus","Tachyglossus","Tadarida","Talpidae",
           "Tamias","Tamiasciurus","Tapirus","Tarsius","Taxidea","Tefia","Tellus","Teralyn","Terra","Terrecea","Terrena",
           "Terrene","Terricia","Tetraodontidae","Tetraoninae","Teuthida","Thomomys","Threskiornithidae","Thylogale",
           "Tragelaphus","Tremarctos","Trichechus","Tridacna","Trochilidae","Troglodytes","Tursiops","Tyto","Urania",
           "Urochordata","Urocyon","Uroplatus","Ursidae","Ursus","Vanessa","Varanus","Vega","Venessa","Vespa","Vespera",
           "Vesperia","Vespira","Viverra","Vombatus","Vulpes","Xenopus","Zapus")
  nm2 <- c("Major","Minor","Australis","Borealis","Occidentalis","Orientalis")
  nm3 <- c("Amaranth","Amber","Amethyst","Aquamarine","Azure","Black","Blue","Brown","Cerulean",
           "Citrine","Cobalt","Crimson","Diamond","Ebony","Emerald","Green","Harlequin","Ivory",
           "Jade","Jasmine","Malachite","Maroon","Onyx","Orange","Pearl","Red","Ruby","Sapphire",
           "Scarlet","Vermilion","Violet","White")
  nm4 <- c("Albatross","Alligator","Ant","Antelope","Horse","Armadillo","Baboon","Badger",
           "Bandicoot","Bat","Bear","Starfish","Beaver","Bee","Bird","Bison","Boar","Buffalo",
           "Butterfly","Cat","Catfish","Chameleon","Chipmunk","Cobra","Cow","Crab","Crane","Crow",
           "Deer","Dog","Donkey","Dove","Dragon","Dragonfly","Duck","Eagle","Elephant","Flamingo",
           "Fox","Frog","Gazelle","Gecko","Gerbil","Goat","Goose","Hare","Hawk","Hedgehog","Hippo",
           "Hummingbird","Hyena","Ibis","Iguana","Jackal","Kangaroo","Koala","Lemur","Leopard","Lion",
           "Lizard","Llama","Lobster","Lynx","Macaw","Mockingbird","Mongoose","Monkey","Mouse","Mule",
           "Ocelot","Ostrich","Otter","Owl","Ox","Parror","Pelican","Penguin","Pheasant","Pig","Pigeon",
           "Porcupine","Rabbit","Raccoon","Rat","Raven","Rhino","Salmon","Seagull","Seal","Serpent","Shark",
           "Sheep","Sloth","Snake","Sparrow","Spider","Squirrel","Stork","Swallow","Swan","Tapir","Tiger",
           "Tortoise","Trout","Turtle","Vulture","Wasp","Whale","Wolf","Yak")
  nm5 <- c("Big","Small","Southern","Northern","Western","Eastern","Tiny","Large")
  nm6 <- c("Helmet","Shield","Sword","Compass","Ship","Hammer","Chisel","Crown","Cup","Cross",
           "River","Furnace","Bridge","Fire","Flame","Gem","Diamond","Clock","Table","Arrow","Temple",
           "Archer","Knight","Telescope","Pyramid","Triangle","Robe","Scarf","Chair","Candle","Tower",
           "House","Castle","Torch","Feather","Rod","Leaf","Tree","Flower","Petal","Droplet","Gate","Spear",
           "Dagger","Fountain","Horn","Tooth","Claw","Lantern","Light","Drum","Cart","Axe","Hatchet","Pickaxe",
           "Eye","Mountain","River","Flute","Wheel","Altar","Skull","Throne","Flag","Boulder","Wave","Maze",
           "Scepter","Soldier","Spade","Wing","Couldron","Broom","Lance","Spear","Tiara","Cannon","Cannonball",
           "Pistol","Rifle","Anvil","Airplane","Wrench","Horseshoe","Needle","Screw","Sapling","Pipe","Hat",
           "Chest","Hourglass","Umbrella","Telephone","Ring","Pencil","Brush","Cone","Scroll","Heart","Spoon",
           "Fork","Knife","Vase","Shell","Book")
  
  type <- sample(1:10,n,replace=TRUE)
  stars <- rep("",n)
  k <- 0
  for(i in type){
    k <- k+1
    if(i<3){
      stars[k] <- sample(nm1,1)
    }
    else if(i<5){
      rnd1 <- sample(nm1,1)
      rnd2 <- sample(nm2,1)
      stars[k] <- paste(rnd1,rnd2)
    }
    else if(i<7){
      rnd1 <- sample(nm3,1)
      rnd2 <- sample(nm4,1)
      stars[k] <- paste(rnd1,rnd2)
    } else{
      rnd1 <- sample(nm5,1)
      rnd2 <- sample(nm6,1)
      stars[k] <- paste(rnd1,rnd2)
    }
  }
  stars
}

#==================================================
get_systemname <- function(){
  ng1 <- c("Alpha","Apus","Aquila","Ara","Beta","Canis","Carina","Comae","Corona","Crux","Delta","Draco","Epsilon",
           "Gamma","Lambda","Lyra","Nemo","Omega","Omicron","Pavo","Proxima","Sagitta","Serpens","Sigma","Theta","Upsilon",
           "Ursa","Vela","Virgo","Zeta")
  ng2 <- c("Acallaris","Achelois","Adastreia","Aegialeus","Aegimius",
           "Alatheia","Alcyoneus","Aldebaran","Amphiaraus","Anadeia","Andromeda",
           "Aquarii","Arcturus","Aristaeus","Asteria","Asteropaios","Astraeus","Aurigae",
           "Boreas","Borysthenis","Calesius","Capella","Cassiopeia","Centauri","Centaurus",
           "Chronos","Cymopoleia","Dioscuri","Draconis","Eioneus","Eridani","Eridanus","Eubuleus",
           "Euphorion","Eusebeia","Euthenia","Hemithea","Hyperbius","Hyperes","Hyperion","Icarius",
           "Ichnaea","Ilioneus","Kentaurus","Leporis","Librae","Lyrae","Majoris","Miriandynus","Myrmidon",
           "Nebula","Nemesis","Odysseus","Ophiuchi","Orion","Orionis","Orithyia","Palioxis","Peleus","Perileos",
           "Perseus","Phoroneus","Polystratus","Porphyrion","Proioxis","Sagittarius","Sirius","Solymus","Zagreus",
           "Zephyrus")
  ng3 <- c("Abyss","Acorn","Arrowhead","Banana","Beansprout","Beanstalk","Bell","Blue Ribbon","Blueberry",
           "Bottleneck","Bowl","Bull's Eye","Bullet","Butterfly","Cat's Ear","Cat's Eye","Catterpillar",
           "Cherry","Chickpea","Clover","Coconut","Comet","Crescent","Crow's Feet","Crown","Dandelion",
           "Diamond","Dragontooth","Droplet","Eagle Claw","Eggshell","Exploding","Eyebrow","Eyelash",
           "Falling","Feather","Fern Leaf","Fingerprint","Fisheye","Fishscale","Flame","Football","Grain",
           "Halo","Heart","Horseshoe","Hurricane","Icicle","Iris","Jellyfish","Kettle","Leaf","Lemon",
           "Lightbulb","Lilypad","Lion's Mane","Lion's Tail","Maelstrom","Meridian","Mosaic","Mouse",
           "Octopus","Oculus","Onion","Owl Head","Pear","Pepper","Pig's Tail","Pinecone","Ponytail",
           "Potato","Red Ribbon","Rippled","Rose Petal","Sawblade","Seashell","Serpent","Serpent's Eye",
           "Sharkfin","Sharktooth","Shield","Shooting Star","Snail Shell","Snowflake","Soap Bubble",
           "Sparrow","Spearpoint","Spiderleg","Spiderweb","Spiral","Starfish","Strawberry","Teacup",
           "Tiara","Tiger Paw","Tree Root","Tree Trunk","Turtle Shell","Vortex","Wave","Whale's Tail","Zodiac")
  ng4 <- c("Nebula","Galaxy","Cloud","Star System")
  ng5 <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","")
  ng6 <- c("0","1","2","3","4","5","6","7","8","9","")
  
  i <- sample(1:7,1)
  if(i<3){
    rnd <- sample(ng1,1)
    rnd2 <- sample(ng2,1)
    paste(rnd,rnd2)
  }else if(i<5){
    rnd <- sample(ng2,1)
    rnd2 <- sample(ng4,1)
    paste(rnd,rnd2)
  }else if(i<8){
    rnd <- sample(ng3,1)
    rnd2 <- sample(ng4,1)
    paste(rnd,rnd2)
  }#else if(i<9){
  #   rnd  <- sample(ng5,1)
  #   rnd2 <- sample(ng5,1)
  #   rnd3 <- sample(ng6,1)
  #   rnd4 <- sample(ng6,1)
  #   rnd5 <- sample(ng6,1)
  #   paste(rnd,rnd2,"-",rnd3,rnd4,rnd5)
  # }else{
  # rnd  <- sample(ng5,1)
  # rnd2 <- sample(ng5,1)
  # rnd3 <- sample(ng5,1)
  # rnd4 <- sample(ng6,1)
  # rnd5 <- sample(ng6,1)
  # paste(rnd,rnd2," ",rnd3,rnd4,rnd5)
  # }
}

get_coord <- function(){
  paste0("RA ",sample(1:23,1),"h ",sample(1:59,1),"m ",sample(1:59,1),"s",", ", "Dec. ", 
         sample(c("+","-"),1),sample(1:90,1),"° ", sample(1:100,1),"′ ", sample(1:100,1),"″")
}
