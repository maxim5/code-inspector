unit Skills;

interface

uses
  GlobalRecordsAndEnums;

type
  TSkills = class
    private
    public
      AmarrEncryption: Integer;
      CaldariEncryption: Integer;
      GallenteEncryption: Integer;
      MinmatarEncryption: Integer;
      AmarrianStarship: Integer;
      CaldariStarship: Integer;
      Electromagnetic: Integer;
      Electronic: Integer;
      GallenteanStarship: Integer;
      Graviton: Integer;
      HighEnergy: Integer;
      Hydromagnetic: Integer;
      Laser: Integer;
      Mechanical: Integer;
      MinmatarStarship: Integer;
      Molecular: Integer;
      Nanite: Integer;
      Nuclear: Integer;
      Plasma: Integer;
      Quantum: Integer;
      Rocket: Integer;
      Science: Integer;
      Industry: Integer;
      ProductionEfficiency: Integer;

      BeancounterF: Integer; // Manufacturing time, 1% 3% 5%
      BeancounterG: Integer; // Manufacturing waste, not available
      BeancounterK: Integer; // Copy time, 1% 3% 5%
      BeancounterCI: Integer; // Invention related, unknown and not available
      Shaman: Integer; // Invention related, unknown and not available
      Cleric: Integer; // Invention related, unknown and not available
      Draftsman: Integer; // Invention related, unknown and not available

{      property AmarrEncryption: Integer read _AmarrEncryption;
      property CaldariEncryption: Integer read _CaldariEncryption;
      property GallenteEncryption: Integer read _GallenteEncryption;
      property MinmatarEncryption: Integer read _MinmatarEncryption;
      property AmarrianStarship: Integer read _AmarrianStarship;
      property CaldariStarship: Integer read _CaldariStarship;
      property Electromagnetic: Integer read _Electromagnetic;
      property Electronic: Integer read _Electronic;
      property GallenteanStarship: Integer read _GallenteanStarship;
      property Graviton: Integer read _Graviton;
      property HighEnergy: Integer read _HighEnergy;
      property Hydromagnetic: Integer read _Hydromagnetic;
      property Laser: Integer read _Laser;
      property Mechanical: Integer read _Mechanical;
      property MinmatarStarship: Integer read _MinmatarStarship;
      property Molecular: Integer read _Molecular;
      property Nanite: Integer read _Nanite;
      property Nuclear: Integer read _Nuclear;
      property Plasma: Integer read _Plasma;
      property Quantum: Integer read _Quantum;
      property Rocket: Integer read _Rocket;
      property Science: Integer read _Science;
      property Industry: Integer read _Industry;
      property ProductionEfficiency: Integer read _ProductionEfficiency;}

      constructor Create;
      procedure Load(skills: SSkills);
      function Save: SSkills;

      function GetSkillLevel(skill: ESkill): Integer;
      function GetBeancounterFModifier: Double;
      function GetBeancounterKModifier: Double;
      function GetProductionEfficiencyModifier: Double;
  end;

implementation

constructor TSkills.Create;
begin
  AmarrEncryption := 0;
  CaldariEncryption := 0;
  GallenteEncryption := 0;
  MinmatarEncryption := 0;
  AmarrianStarship := 0;
  CaldariStarship := 0;
  Electromagnetic := 0;
  Electronic := 0;
  GallenteanStarship := 0;
  Graviton := 0;
  HighEnergy := 0;
  Hydromagnetic := 0;
  Laser := 0;
  Mechanical := 0;
  MinmatarStarship := 0;
  Molecular := 0;
  Nanite := 0;
  Nuclear := 0;
  Plasma := 0;
  Quantum := 0;
  Rocket := 0;
  Science := 0;
  Industry := 0;
  ProductionEfficiency := 0;

  BeancounterF := 0;
  BeancounterG := 0;
  BeancounterK := 0;
  BeancounterCI := 0;
  Shaman := 0;
  Cleric := 0;
  Draftsman := 0;
end;

procedure TSkills.Load(skills: SSkills);
begin
  self.AmarrEncryption := skills.AmarrEncryption;
  self.CaldariEncryption := skills.CaldariEncryption;
  self.GallenteEncryption := skills.GallenteEncryption;
  self.MinmatarEncryption := skills.MinmatarEncryption;
  self.AmarrianStarship := skills.AmarrianStarship;
  self.CaldariStarship := skills.CaldariStarship;
  self.Electromagnetic := skills.Electromagnetic;
  self.Electronic := skills.Electronic;
  self.GallenteanStarship := skills.GallenteanStarship;
  self.Graviton := skills.Graviton;
  self.HighEnergy := skills.HighEnergy;
  self.Hydromagnetic := skills.Hydromagnetic;
  self.Laser := skills.Laser;
  self.Mechanical := skills.Mechanical;
  self.MinmatarStarship := skills.MinmatarStarship;
  self.Molecular := skills.Molecular;
  self.Nanite := skills.Nanite;
  self.Nuclear := skills.Nuclear;
  self.Plasma := skills.Plasma;
  self.Quantum := skills.Quantum;
  self.Rocket := skills.Rocket;
  self.Science := skills.Science;
  self.Industry := skills.Industry;
  self.ProductionEfficiency := skills.ProductionEfficiency;

  self.BeancounterF := skills.BeancounterF;
  self.BeancounterG := skills.BeancounterG;
  self.BeancounterK := skills.BeancounterK;
  self.BeancounterCI := skills.BeancounterCI;
  self.Shaman := skills.Shaman;
  self.Cleric := skills.Cleric;
  self.Draftsman := skills.Draftsman;
end;

function TSkills.Save: SSkills;
begin
  result.AmarrEncryption := self.AmarrEncryption;
  result.CaldariEncryption := self.CaldariEncryption;
  result.GallenteEncryption := self.GallenteEncryption;
  result.MinmatarEncryption := self.MinmatarEncryption;
  result.AmarrianStarship := self.AmarrianStarship;
  result.CaldariStarship := self.CaldariStarship;
  result.Electromagnetic := self.Electromagnetic;
  result.Electronic := self.Electronic;
  result.GallenteanStarship := self.GallenteanStarship;
  result.Graviton := self.Graviton;
  result.HighEnergy := self.HighEnergy;
  result.Hydromagnetic := self.Hydromagnetic;
  result.Laser := self.Laser;
  result.Mechanical := self.Mechanical;
  result.MinmatarStarship := self.MinmatarStarship;
  result.Molecular := self.Molecular;
  result.Nanite := self.Nanite;
  result.Nuclear := self.Nuclear;
  result.Plasma := self.Plasma;
  result.Quantum := self.Quantum;
  result.Rocket := self.Rocket;
  result.Science := self.Science;
  result.Industry := self.Industry;
  result.ProductionEfficiency := self.ProductionEfficiency;

  result.BeancounterF := self.BeancounterF;
  result.BeancounterG := self.BeancounterG;
  result.BeancounterK := self.BeancounterK;
  result.BeancounterCI := self.BeancounterCI;
  result.Shaman := self.Shaman;
  result.Cleric := self.Cleric;
  result.Draftsman := self.Draftsman;
end;

function TSkills.GetSkillLevel(skill: ESkill): Integer;
begin
  case skill of
    SK_AmarrEncryption: result := self.AmarrEncryption;
    SK_CaldariEncryption: result := self.CaldariEncryption;
    SK_GallenteEncryption: result := self.GallenteEncryption;
    SK_MinmatarEncryption: result := self.MinmatarEncryption;
    SK_AmarrianStarship: result := self.AmarrianStarship;
    SK_CaldariStarship: result := self.CaldariStarship;
    SK_Electromagnetic: result := self.Electromagnetic;
    SK_Electronic: result := self.Electronic;
    SK_GallenteanStarship: result := self.GallenteanStarship;
    SK_Graviton: result := self.Graviton;
    SK_HighEnergy: result := self.HighEnergy;
    SK_Hydromagnetic: result := self.Hydromagnetic;
    SK_Laser: result := self.Laser;
    SK_Mechanical: result := self.Mechanical;
    SK_MinmatarStarship: result := self.MinmatarStarship;
    SK_Molecular: result := self.Molecular;
    SK_Nanite: result := self.Nanite;
    SK_Nuclear: result := self.Nuclear;
    SK_Plasma: result := self.Plasma;
    SK_Quantum: result := self.Quantum;
    SK_Rocket: result := self.Rocket;
    SK_Science: result := self.Science;
    SK_Industry: result := self.Industry;
    SK_ProductionEfficiency: result := self.ProductionEfficiency;
  else result := 0;
  end;
end;

function TSkills.GetBeancounterFModifier: Double;
// Manufacturing time
begin
  case BeancounterF of
    1: result := 0.99;
    2: result := 0.97;
    3: result := 0.95;
  else result := 1;
  end;
end;

function TSkills.GetBeancounterKModifier: Double;
// Copy time
begin
  case BeancounterK of
    1: result := 0.99;
    2: result := 0.97;
    3: result := 0.95;
  else result := 1;
  end;
end;

function TSkills.GetProductionEfficiencyModifier: Double;
begin
  result := 1.25 - (0.05 * ProductionEfficiency);
end;

end.
