%FF Correction Script
%Tom Bell
%7/20/2015

%Make a coords array of the corner coordinates of the image
%This should be grabbed from the metadata file
%The first column is Lats and the second is Lons for a 4x2 array

%This function transforms the degrees to utm
%The number at the end of the inputs is the utm zone
%SoCal is zone 11 and cencal is zone 10
    [Lonu,Latu,utm] = ll2utm(coords(:,1), coords(:,2),11);

%This makes an array of 30m Lats and Lons in utm format
%You can add 15 to each to make the location in the center of the pixel
%Here the location is the upper left corner
    [Lon, Lat] = meshgrid(min(Lonu):30:max(Lonu),min(Latu):30:max(Latu));
    Lat = flipud(Lat);

%This just calculated the height and width of each FF image
%To my knowledge the FF script treats every Landsat image as the same size
%While this is not true, it does produce evenly sized FF images
%I think it is fine to keep this part as is for now
    no_rows  = 20;
    no_colls = 20;
    image_width  = 7981;
    image_height = 7271;
    image_chunk_width = image_width / no_rows; %400 pixels
    image_chunk_height = image_height / no_colls ; %364 pixels
  
%This generates the boundaries for each FF image in the Landsat image
    Height = 1:363:size(Lat,1);
    Width = 1:399:size(Lat,2);
   
 R = 1;   
  for j = 1:(length(Width) - 1)
      
      for i = 1:(length(Height) - 1)
          
        %Creates array of Lats and Lons for each FF image
        LatSlice = Lat(Height(i):Height(i+1),Width(j):Width(j+1),:);
        LonSlice = Lon(Height(i):Height(i+1),Width(j):Width(j+1),:);
  
        %Resizes the Lats and Lons to the FF resized image (484x532 pixels)   
        Diff = max(LatSlice(:)) - min(LatSlice(:));
        DiffE = Diff/483;
        LatResize = min(LatSlice(:)):DiffE:max(LatSlice(:));
        LatResize = LatResize(:);
        LatResize = repmat(LatResize,1,532);
  
        Diff = max(LonSlice(:)) - min(LonSlice(:));
        DiffE = Diff/531;
        LonResize = min(LonSlice(:)):DiffE:max(LonSlice(:));
        LonResize = repmat(LonResize,484,1);
  
        %Outputs a csv with the coords for each FF image in UTM
        EE = num2str(R);
        OutLat = ['/Users/tbell/Desktop/FF_Coords/' EE '_y_utm.csv'];
        OutLon = ['/Users/tbell/Desktop/FF_Coords/' EE '_x_utm.csv'];
        csvwrite(OutLat, LatResize)
        csvwrite(OutLon, LonResize)
        
        %Transforms UTM to degrees for each resized image
%         AAA = utm2ll(LonResize(:),LatResize(:),10);
%         LatSliceLL = reshape(AAA(:,1),484,532);
%         LonSliceLL = reshape(AAA(:,2),484,532);
        
        %Outputs a csv with the coords for the image in degrees
        %May have to set precision with this one
%         OutLatd = ['/Users/tbell/Desktop/FF_Coords/' EE '_y_degree.csv'];
%         OutLond = ['/Users/tbell/Desktop/FF_Coords/' EE '_x_degree.csv'];
%         csvwrite(OutLatd, LatSliceLL)
%         csvwrite(OutLond, LonSliceLL)
        
        R = R + 1;
      end
  end
  
  clear i j Latu Lonu utm Lon Lat no_rows no_colls image_width image_height
  clear image_chunk_width image_chunk_height Height Width R LatSlice 
  clear LonSlice AAA OutLat OutLon OutLatd OutLond Diff DiffE LatResize
  clear LonResize EE LatSliceLL LonSliceLL 
  
  
  
  
  
  
  
  
  
  %If you ever wanted to reproduce the FF images from the Landsat image as a
%sanity check, you can do so with this script, just download the image and 
%change the directories

% [NIR R] = geotiffread('/Users/tbell/Downloads/LE70440351999204EDC01/LE70440351999204EDC01_B4.tif');
% [Red R] = geotiffread('/Users/tbell/Downloads/LE70440351999204EDC01/LE70440351999204EDC01_B3.tif');
% [Blue R] = geotiffread('/Users/tbell/Downloads/LE70440351999204EDC01/LE70440351999204EDC01_B2.tif');
% RGB = cat(3,Red,NIR,Blue);

%   R = 1;
%   
%   for j = 1:(length(Width) - 1)
%       for i = 1:(length(Height) - 1)
%       Cut = RGB(Height(i):Height(i+1),Width(j):Width(j+1),:);
%       EE = num2str(R);
%       Out = ['/Users/tbell/Desktop/FF_Pics/' EE '.png'];
%       Cut2 = imresize(Cut, 1.329);
%       imwrite(Cut2,Out)
%       R = R+1;
%       end
%   end
  