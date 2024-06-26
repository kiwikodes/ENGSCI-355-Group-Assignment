Define ObjectType {
	FIFOQControlUnit
	RadiologyExtendedControlUnit
	HospitalControlUnit
	EDControlUnit
	PTControlUnit
	WardsControlUnit
}

ControllerIconModel ImageFile { '<res>/images/Controller-256.png' }  Transparent { TRUE }
AssembleIconModel ImageFile { '<res>/images/Assemble-256.png' }  Transparent { TRUE }

FIFOQControlUnit JavaClass { ssq.FIFOQControlUnit }  Palette { 'Single Server Queue' } DefaultDisplayModel { ControllerIconModel } IconFile { '<res>/images/Controller-24.png' } DefaultSize { 0.5 0.5 0.5 m }
RadiologyExtendedControlUnit JavaClass { labs.RadiologyExtendedControlUnit }  Palette { 'Custom Logic' } DefaultDisplayModel { AssembleIconModel } IconFile { '<res>/images/Assemble-24.png' } DefaultSize { 0.5 0.5 0.5 m }
HospitalControlUnit JavaClass { assignment.HospitalControlUnit }  Palette { 'Custom Logic' } DefaultDisplayModel { AssembleIconModel } IconFile { '<res>/images/Assemble-24.png' } DefaultSize { 0.5 0.5 0.5 m }
EDControlUnit JavaClass { hospital_sim.EDControlUnit } Palette { 'Custom Logic' } DefaultDisplayModel { AssembleIconModel } IconFile { '<res>/images/Assemble-24.png' } DefaultSize { 0.5 0.5 0.5 m }
PTControlUnit JavaClass { hospital_sim.PTControlUnit } Palette { 'Custom Logic' } DefaultDisplayModel { AssembleIconModel } IconFile { '<res>/images/Assemble-24.png' } DefaultSize { 0.5 0.5 0.5 m }
WardsControlUnit JavaClass { hospital_sim.WardsControlUnit } Palette { 'Custom Logic' } DefaultDisplayModel { AssembleIconModel } IconFile { '<res>/images/Assemble-24.png' } DefaultSize { 0.5 0.5 0.5 m }
