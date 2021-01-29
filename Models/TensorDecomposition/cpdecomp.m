%%
%clean the environment
clear all;


%%
%path of ndtf directory

dirpath = '~/Dropbox/2020 Spring/ndtf/';
datapath = strcat(dirpath,'new_datasets/');
indpath = strcat(dirpath,'ind/');
facpath = strcat(dirpath,'factorized_data17/');


%dirpath = '~/Dropbox/2020 Spring/ndtf/'
%datapath = strcat(dirpath,'new_datasets/');
%indpath = strcat(dirpath,'ind/');
%facpath = strcat(dirpath,'factorized_data/');


%%

%load data by csvread

GE = csvread(strcat(datapath,'new_ge.csv'),1,0);
ME = csvread(strcat(datapath,'new_me.csv'),1,0);
CN = csvread(strcat(datapath,'new_cn.csv'),1,0);

%%

%load data by csvread

GE = GE';
ME = ME';
CN = CN';

%GE = zscore(GE,0,1);
%ME = zscore(ME,0,1);
%CN = zscore(CN,0,1);


%%


for ind = 1:20
    trainind = csvread(strcat(indpath,'trainind',num2str(ind),'.csv'),1,1);
    testind =  csvread(strcat(indpath,'testind',num2str(ind),'.csv'),1,1);
    [ntest ncol] = size(testind);
    %build temp 3d-matrix for training
    Temptrain(:,:,1) = GE(trainind,:);
    Temptrain(:,:,2) = ME(trainind,:);
    Temptrain(:,:,3) = CN(trainind,:);
    %convert matrix into tensor structure
    tensor_train = tensor(Temptrain);
    
    %build temp 3d-matrix for testing
    Temptest(:,:,1) = GE(testind,:);
    Temptest(:,:,2) = ME(testind,:);
    Temptest(:,:,3) = CN(testind,:);
    %convert matrix into tensor structure
    tensor_test = tensor(Temptest);
    
    r = 17
    %cpals for training tensor
    trainfac = cp_als(tensor_train,r);

    A = trainfac.u{1};
    B = trainfac.u{2};
    C = trainfac.u{3};
    lam = trainfac.lambda.';

    replam = repmat(lam,ntest,1);
    %cpals for testing tensor

    BC = pinv(kr(C,B).');

    [nbc ncol2] = size(BC);

    %mode 1 x test
    X1 = reshape(Temptest,ntest,nbc);

    %estimated value of A on test set data
    A_hat = X1 * BC;

    %normalize A_hat
    A_hat_norm = A_hat ./ replam;

    %save sample by rank matrix

    to_write_name = strcat(facpath,'17train',num2str(ind),'rank',num2str(r),'.csv');
    csvwrite(char(to_write_name),trainfac.u{1} );
    to_write_name = strcat(facpath,'17test',num2str(ind),'rank',num2str(r),'.csv');
    csvwrite(char(to_write_name),A_hat_norm);
    
end   
    clear Temptrain;
    clear Temptest;

