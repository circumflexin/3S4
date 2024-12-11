function [file, time_into_chunk] = get_wavtime(cue, ctab)
% convert a cue time to a location in the audio

    ctab_dt = datetime(ctab.Var1, ctab.Var2, ctab.Var3, ctab.Var4, ctab.Var5, ctab.Var6);
    bool = ctab_dt < cue;
    ct = max(ctab_dt(bool));
    ci = find(ctab_dt == ct);
    file = ctab.Var7{ci};
    time_into_chunk = cue - ct;
end
