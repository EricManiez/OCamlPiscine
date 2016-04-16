class water =
	object
		inherit Abstract.molecule "Water" [new Atoms.hydrogen ; new Atoms.hydrogen ; new Atoms.oxygen]
	end

class carbon_dioxyde =
	object
		inherit Abstract.molecule "Carbon dioxyde" [new Atoms.carbon ; new Atoms.oxygen ; new Atoms.oxygen]
	end

class ethanol =
	object
		inherit Abstract.molecule "Ethanol" [
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.oxygen
		]
	end

class polyethylene =
	object
		inherit Abstract.molecule "Polyethylene" [
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
		]
	end

class methane =
	object
		inherit Abstract.molecule "Methane" [
			new Atoms.carbon ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
		]
	end

class methanethiol =
	object
		inherit Abstract.molecule "Methanethiol" [
			new Atoms.carbon ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.sulfur ;
		]
	end

class sulfuric_acid =
	object
		inherit Abstract.molecule "Sulfuric Acid" [
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.sulfur ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
		]
	end

class megaphone =
	object
		inherit Abstract.molecule "Megaphone" [
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
		]
	end

class trinitrotoluene =
	object
		inherit Abstract.molecule "Trinitrotoluene" [
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.carbon ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
			new Atoms.nitrogen ;
			new Atoms.nitrogen ;
			new Atoms.nitrogen ;
		]
	end

class lithium_fluoride =
	object
		inherit Abstract.molecule "Lithium Fluoride" [
			new Atoms.lithium ;
			new Atoms.fluorine ;
		]
	end

class dilithium_peroxide =
	object
		inherit Abstract.molecule "Dilithium Peroxide" [
			new Atoms.lithium ;
			new Atoms.lithium ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
		]
	end

class lithium_hydride =
	object
		inherit Abstract.molecule "Lithium Hydride" [
			new Atoms.lithium ;
			new Atoms.lithium ;
			new Atoms.hydrogen ;
		]
	end

class boric_acid =
	object
		inherit Abstract.molecule "Boric Acid" [
			new Atoms.boron ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.hydrogen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
			new Atoms.oxygen ;
		]
	end

class boron_carbide =
	object
		inherit Abstract.molecule "Boric Acid" [
			new Atoms.boron ;
			new Atoms.boron ;
			new Atoms.boron ;
			new Atoms.boron ;
			new Atoms.carbon ;
		]
	end

