unit gtreeproc;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}


interface

uses
  Classes, SysUtils, gtreenode;

procedure SwapNode(VAR p1,p2 : TpNode);
function SibNode(p : TpNode):TpNode;
procedure SetMaxLength(root : TpNode);
procedure SetClusterSize(root : TpNode);
procedure SetDepth(root : TpNode; Threshold : integer);
procedure SortBranchByFigure(p : TpNode);
procedure SortBranchByOrder(p : TpNode);
function SearchCommonAncestor(root : TpNode):TpNode;
procedure ChangeRoot(root, newposition : TpNode; midpoint : boolean);

implementation

uses
  Math;

procedure SwapNode(VAR p1,p2 : TpNode);
var p : TpNode;
begin
    p := p1;
    p1 := p2;
    p2 := p;
end;

function SibNode(p : TpNode):TpNode;
begin
    if p.anc = nil then
        Result := nil
    else if p = p.anc.des1 then
        Result := p.anc.des2
    else if p = p.anc.des2 then
        Result := p.anc.des1
    else
        Result := nil;
end;

procedure SetMaxLength(root : TpNode);
var q : TpNode;

    procedure SetMaxLen2(p : TpNode);
    begin
        if p.OTU then
            p.branch.maxlen2 := p.branch.length
        else begin
            SetMaxLen2(p.des1);
            SetMaxLen2(p.des2);
            if p.des1.branch.maxlen2 > p.des2.branch.maxlen2 then
                p.branch.maxlen2 := p.des1.branch.maxlen2 + p.branch.length
            else
                p.branch.maxlen2 := p.des2.branch.maxlen2 + p.branch.length;
        end;
    end; { SetMaxLen2 }

    procedure SetMaxLen1(p : TpNode);
    begin
        q := SibNode(p);
        if p.anc.branch.maxlen1 > q.branch.maxlen2 then
            p.branch.maxlen1 := p.anc.branch.maxlen1 +p.branch.length
        else
            p.branch.maxlen1 := q.branch.maxlen2 +p.branch.length;
        if not p^.OTU then begin
            SetMaxLen1(p^.des1);
            SetMaxLen1(p^.des2);
        end;
    end; { SetMaxLen1 }

begin
    with root^ do begin
        SetMaxLen2(des1);
        SetMaxLen2(des2);
        des1.branch.maxlen1 := des2.branch.maxlen2 + des1.branch.length;
        des2.branch.maxlen1 := des1.branch.maxlen2 + des2.branch.length;
        if not des1.OTU then begin
            SetMaxLen1(des1.des1);
            SetMaxLen1(des1.des2);
        end;
        if not des2.OTU then begin
            SetMaxLen1(des2.des1);
            SetMaxLen1(des2.des2);
        end;
    end;
end; { SetMaxLength }

procedure SetDepth(root : TpNode; Threshold : integer);

    procedure SetDepthFromOTU(p : TpNode; Threshold : integer);
    var d1,d2 : integer;
    begin
        with p^ do begin
            if OTU or compressed then
                depth := 0
            else begin
                SetDepthFromOTU(des1, Threshold);
                SetDepthFromOTU(des2, Threshold);
                if (des1.OTU or des1.compressed) or (des1.branch.stat2 >= Threshold) then
                    d1 := des1.depth +1
                else
                    d1 := des1.depth;
                if (des2.OTU or des2.compressed) or (des2.branch.stat2 >= Threshold) then
                    d2 := des2.depth +1
                else
                    d2 := des2.depth;
                if d1 > d2 then
                    depth := d1
                else
                    depth := d2;
            end;
        end;
    end;

    procedure SetDepthFromRoot(p : TpNode; Threshold : integer);
    begin
        with p^ do begin
            if OTU then Exit;
            if not((anc = root) and
                   (anc.des1.OTU or anc.des2.OTU or anc.des1.compressed or anc.des2.compressed)) then
                if branch.stat2 < Threshold then
                    depth := anc^.depth;
            SetDepthFromRoot(des1, Threshold);
            SetDepthFromRoot(des2, Threshold);
        end;
    end;

begin
    SetDepthFromOTU(root, Threshold);
    with root^ do
    begin
        if des1.OTU or des2.OTU or des1.compressed or des2.compressed then
            if des1.depth > des2.depth then
          if des1.branch.stat2 >= Threshold then
                depth := des1.depth +1
            else
            depth := des1.depth
        else if des2.branch.stat2 >= Threshold then
          depth := des2.depth +1
        else
          depth := des2.depth;

        SetDepthFromRoot(des1, Threshold);
        SetDepthFromRoot(des2, Threshold);
    end;
end;

procedure SetClusterSize(root : TpNode);
begin
    with root^ do
        if not OTU then begin
            SetClusterSize(des1);
            SetClusterSize(des2);
            size := des1^.size + des2^.size;
            if des1^.minOTU < des2^.minOTU then
                minOTU := des1^.minOTU
            else
                minOTU := des2^.minOTU;
        end;
end; { SetClusterSize }

function SearchCommonAncestor(root : TpNode):TpNode;
var maxsize : integer;

    procedure SetNodeFlagFromOTU(p : TpNode);
    begin
        if p.OTU then Exit;
        SetNodeFlagFromOTU(p.des1);
        SetNodeFlagFromOTU(p.des2);
        if p.des1.flag or p.des2.flag then
            p.flag := true
        else
            p.flag := false;
    end;

    procedure SetNodeFlagFromRoot(p : TpNode);
    begin
        if p.OTU or (p.des1.flag and p.des2.flag) then Exit;
        if p.des1.flag or p.des2.flag then begin
            p.flag := false;
            if p.des1.flag then
                SetNodeFlagFromRoot(p.des1);
            if p.des2.flag then
                SetNodeFlagFromRoot(p.des2);
        end;
    end;

    procedure FindAncestor(p : TpNode; f : boolean);
    begin
        if p.flag = f then begin
            if p.size > maxsize then begin
                Result := p;
                maxsize := p.size;
            end
        end
        else if not p.OTU then begin
            FindAncestor(p.des1, f);
            FindAncestor(p.des2, f);
        end;
    end;

begin
    SetNodeFlagFromOTU(root);
    if not root.flag then begin
        Result := root;
        Exit;
    end;
    SetNodeFlagFromRoot(root);
    maxsize := 0;
    if root.flag then
        FindAncestor(root, false)
    else begin
        FindAncestor(root, true);
        if Result.size > (root.size div 2) then begin
            maxsize := root.size -Result.size;
            FindAncestor(Result, false);
        end;
    end;
end;

procedure ChangeRoot(root, newposition : TpNode; midpoint : boolean);
var a, p, d, q : TpNode;
    b0,b1,b2 : TBranch;
    rootmode: integer;

    procedure SwapMaxlen(var b : TBranch);
    var len : double;
    begin
        len := b.maxlen1;
        b.maxlen1 := b.maxlen2;
        b.maxlen2 := len;
    end;

begin
    if newposition = root then Exit;
    if newposition^.anc = root then
    begin
        if midpoint then
            if (newposition = root.des1) and (root.des1.branch.maxlen2 < root.des2.branch.maxlen2) then
                newposition := root.des2
            else if (newposition = root.des2) and (root.des2.branch.maxlen2 < root.des1.branch.maxlen2) then
                newposition := root.des1;
        with newposition^ do
        begin
            if midpoint then
                if ABS(branch.maxlen1 -branch.maxlen2) <= branch.length then
                begin
                    b0.length := (branch.maxlen1 +branch.maxlen2-branch.length)/2;
                    b1.length := branch.maxlen2 - b0.length;
                    b2.length := branch.maxlen1 - b0.length;
                end
                else
                begin
                    b1.length := 0.0;
                    b2.length := branch.length;
                end
            else
            begin
                b1.length := branch.length*1/10;
                b2.length := branch.length*9/10;
            end;
            if newposition = root.des1 then
            begin
                root.des2.branch.length  := root.des2.branch.length  +b1.length;
                root.des2.branch.maxlen2 := root.des2.branch.maxlen2 +b1.length;
                branch.length := b2.length;
                branch.maxlen2 := branch.maxlen2 -b1.length;
            end
            else if newposition = root.des2 then
            begin
                root.des1.branch.length  := root.des1.branch.length  +b1.length;
                root.des1.branch.maxlen2 := root.des1.branch.maxlen2 +b1.length;
                branch.length  := b2.length;
                branch.maxlen2 := branch.maxlen2 -b1.length;
            end;
        end;
        Exit;
    end;

    p := newposition;
    while p.anc <> root do
        p := p.anc;

    if p = p.anc.des1 then
        rootmode := 1
    else
        rootmode := 2;
    p := newposition;
    if rootmode = 1 then
    begin
      while p.anc <> root do
      begin
        if p = p.anc.des1 then
          SwapNode(p.anc.des1, p.anc.des2);
        p := p.anc;
      end;
    end
    else
    begin
      while p.anc <> root do
      begin
        if p = p.anc.des2 then
          SwapNode(p.anc.des1, p.anc.des2);
        p := p.anc;
      end;
    end;

    q := SibNode(p);
    b0 := q.branch;
    b0.maxlen2 := p.branch.maxlen1;
    b0.length := root.des1.branch.length +root.des2.branch.length;
    b0.SE     := root.des1.branch.SE +root.des2.branch.SE;
    b0.stats  := max(root.des1.branch.stats, root.des2.branch.stats);
    root.des1.branch.stats := b0.stats;
    root.des2.branch.stats := b0.stats;
    b0.stat2  := max(root.des1.branch.stat2, root.des2.branch.stat2);
    root.des1.branch.stat2 := b0.stat2;
    root.des2.branch.stat2 := b0.stat2;

    d := newposition;
    p := d.anc;
    a := p.anc;
    b2 := d.branch;
    while p <> root do
    begin
        b1 := p.branch;
        p.branch := b2;
        SwapMaxlen(p.branch);
        b2 := b1;
        p.anc := d;
        if d = p.des1 then
            p.des1 := a
        else
            p.des2 := a;
        d := p;
        p := a;
        a := a^.anc;
    end;
    if d = p.des1 then
    begin
        p.des2.anc := d;
        p.des2.branch := b0;
        if p = d.des1 then
            d.des1 := p.des2
        else
            d.des2 := p.des2;
    end
    else
    begin
        p.des1.anc := d;
        p.des1.branch := b0;
        if p = d.des1 then
            d.des1 := p.des1
        else
            d.des2 := p.des1;
    end;

    p := newposition.anc;
    p.anc := root;
    newposition.anc := root;
    if rootmode = 1 then
    begin
        root.des1 := newposition;
        root.des2 := p;
    end
    else
    begin
        root.des1 := p;
        root.des2 := newposition;
    end;
    b0 := newposition.branch;
    b1 := b0;
    b2 := b0;
    if ABS(b0.maxlen1 -b0.maxlen2) <= b0.length then
    begin
        b1.maxlen1 := (b0.maxlen1+b0.maxlen2-b0.length)/2;
        b1.length := b0.maxlen2 - b1.maxlen1;
        b2.length := b0.maxlen1 - b1.maxlen1;
    end
    else
    begin
        b1.length := b0.length*b0.maxlen2/(b0.maxlen1 +b0.maxlen2);
        b2.length := b0.length*b0.maxlen1/(b0.maxlen1 +b0.maxlen2);
    end;

    b1.maxlen1 := b0.maxlen2;
    b1.maxlen2 := b0.maxlen1 -b2.length;
    b2.maxlen1 := b0.maxlen1;
    b2.maxlen2 := b0.maxlen2 -b1.length;

    p.branch := b1;
    newposition.branch := b2;
end;

procedure SortBranchByFigure(p : TpNode);
begin
    if p^.OTU then Exit;
    if p^.anc = nil then begin
        if p^.des1^.size < p^.des2^.size then
            SwapNode(p^.des1, p^.des2);
    end
    else
        if p = p^.anc^.des1 then begin
            if p^.des1^.size < p^.des2^.size then
                SwapNode(p^.des1, p^.des2);
        end
        else if p = p^.anc^.des2 then begin
            if p^.des1^.size > p^.des2^.size then
                 SwapNode(p^.des1, p^.des2);
        end;
    if p^.des1^.size = p^.des2^.size then
        if p^.des1^.minOTU > p^.des2^.minOTU then
            SwapNode(p^.des1, p^.des2);
    SortBranchByFigure(p^.des1);
    SortBranchByFigure(p^.des2);

end;

procedure SortBranchByOrder(p : TpNode);
begin
    if p^.OTU then Exit;
    if p^.des1^.minOTU > p^.des2^.minOTU then
        SwapNode(p^.des1,p^.des2);
    SortBranchByOrder(p^.des1);
    SortBranchByOrder(p^.des2);
end;

end.

