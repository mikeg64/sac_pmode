 filename='/fastdata/cs1mkg/sac/p5b0_1_bv20g/asc/3D_spic_128_bv20G_70.out';
A = importdata(filename,' ',5);

%extract properties from A.textdata
  idim=3;
  nw=12;
  
 % wx=reshape(A.data, 16,128,128,128)
   
%    for idim=1:ndim
%       X(:,idim)=fread(fid,nxs,'float64');
%    end
%    
%    for iw=1:nw
%       %fread(fid,4);
%       w(:,iw)=fread(fid,nxs,'float64');
%       %fread(fid,4);
%    end
w=zeros(12,128,128,128);
   for iw=4:16
      %fread(fid,4);
      tmp=A.data(:,iw);
      tmp=reshape(tmp,[128,128,128,1]);
      w(iw-3,:,:,:)=tmp;
      %fread(fid,4);
   end
   
   X=zeros(3,128,128,128);
   for iw=1:3
      %fread(fid,4);
      tmp=A.data(:,iw);
      tmp=reshape(tmp,[128,128,128,1]);
      X(iw,:,:,:)=tmp;
      %fread(fid,4);
   end
   
   
   
   
   surf(w(4,:,:,1));
   
   val=reshape(w(4,:,:,1),128,128);
   surf(val,'LineStyle','none');
   colorbar;
   
   
   