(import ./.).shellFor {
  packages = p: [ p.bimaps p.BiobaseTypes p.BiobaseXNA p.DPutils p.ForestStructures p.PrimitiveArray p.SciBaseTypes ];
  withHoogle = true;
}
