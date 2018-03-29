function interface= updateinterface(node,interface, idx, NC)

T = auxstructure(interface);
edge2elem = double(T.edge2elem);
clear T;

NT = size(interface, 1);
interface2idx = sparse(1:NT, idx, 1, NT, NC);
interfaceType = full(sum(interface2idx,1))';

updateinterface2();
updateinterface3();
updateinterface4();


    function updateinterface2()
        [square, I] = getpolygon(2);
        localTri = [1, 2, 3, 3, 4, 1; 2, 3, 4, 4, 1, 2];
        NS = size(square,1);
        maxa = zeros(NS, 2);
        for i = 1:2
            elem = reshape(square(:,localTri(i,:))', 3,[])';
            aa = max(myangle(elem), [], 2);
            maxa(:,i) = max(reshape(aa,2,[]), [], 1)';
        end
        
        [~, j] = min(maxa,[],2);
        interface(I,:) = reshape(square(cumsum(ones(NS,6),1) + NS*(localTri(j,:) - 1))', 3, [])';
        
    end


    function updateinterface3()      
        [pentagon,I] = getpolygon(3);
        localTri = [
            1, 2, 3, 1, 3, 4, 1, 4, 5
            2, 3, 4, 2, 4, 5, 2, 5, 1
            3, 4, 5, 3, 5, 1, 3, 1, 2
            4, 5, 1, 4, 1, 2, 4, 2, 3
            5, 1, 2, 5, 2, 3, 5, 3, 4];
        NP = size(pentagon,1);
        maxa = zeros(NP,5);
        for i = 1:5
            elem = reshape(pentagon(:,localTri(i,:))', 3,[])';
            aa = max(myangle(elem), [], 2);
            maxa(:,i) = max(reshape(aa,3,[]), [], 1)';
        end
        
        [~, j] = min(maxa,[],2);
        interface(I,:) = reshape(pentagon(cumsum(ones(NP,9),1) + NP*(localTri(j,:) - 1))', 3, [])';
    end

    function updateinterface4()
        [hexagon, I] = getpolygon(4); 
        NH = size(hexagon, 1);
        localTri = [
            1, 2, 6, 3, 4, 5, 2, 5, 6, 3, 5, 2
            1, 2, 6, 3, 4, 5, 2, 3, 6, 3, 5, 6
            1, 2, 3, 4, 5, 6, 3, 4, 6, 3, 6, 1
            1, 2, 3, 4, 5, 6, 3, 4, 1, 4, 6, 1
            1, 2, 3, 3, 4, 5, 5, 6, 1, 1, 3, 5
            1, 2, 3, 1, 5, 6, 1, 3, 4, 1, 4, 5
            1, 2, 3, 3, 4, 5, 3, 5, 6, 3, 6, 1
            3, 4, 5, 5, 6, 1, 2, 5, 1, 3, 5, 2
            1, 2, 6, 2, 3, 4, 4, 5, 6, 2, 4, 6
            1, 2, 6, 2, 3, 4, 2, 4, 5, 2, 5, 6
            2, 3, 4, 4, 5, 6, 4, 6, 1, 4, 1, 2
            1, 2, 6, 4, 5, 6, 2, 3, 6, 3, 4, 6];
        maxa = zeros(NH,12);
        for i = 1:12
            elem = reshape(hexagon(:,localTri(i,:))', 3,[])';
            aa = max(myangle(elem),[],2);
            maxa(:,i) = max(reshape(aa,4,[]), [], 1)';
        end
        
        [~, j] = min(maxa,[],2);
        interface(I,:) = reshape(hexagon(cumsum(ones(NH,12),1) + NH*(localTri(j,:) - 1))', 3, [])';
    end
        
        

    function a = myangle(elem)
        v1 = node(elem(:,3),:) - node(elem(:,2),:);
        v2 = node(elem(:,1),:) - node(elem(:,3),:);
        v3 = node(elem(:,2),:) - node(elem(:,1),:);
        
        s1 = sqrt(sum(v1.^2,2));
        s2 = sqrt(sum(v2.^2,2));
        s3 = sqrt(sum(v3.^2,2));
        d1 = s2.^2 + s3.^2 - s1.^2;
        d2 = s3.^2 + s1.^2 - s2.^2;
        d3 = s1.^2 + s2.^2 - s3.^2;
        a1=acosd(d1./(2*s2.*s3));
        a2=acosd(d2./(2*s3.*s1));
        a3=acosd(d3./(2*s1.*s2));
        a = [a1,a2, a3];
    end

    function [polygon,I] = getpolygon(numtri)
        i2i = interface2idx(:, interfaceType == numtri);
        [I, J] = find(i2i);
        next = [ 2;3;1];
        prev = [ 3;1;2];
        elem2idx = zeros(NT, 1);
        elem2idx(I) = J;
        isEdge1 = elem2idx(edge2elem(:, 1)) & idx(edge2elem(:, 1)) ~= idx(edge2elem(:, 2));
        isEdge2 = elem2idx(edge2elem(:, 2)) & idx(edge2elem(:, 1)) ~= idx(edge2elem(:, 2));
        idx1 = edge2elem(isEdge1,1) + NT*(next(edge2elem(isEdge1, 3)) - 1);
        idx2 = edge2elem(isEdge1,1) + NT*(prev(edge2elem(isEdge1, 3)) - 1);
        edge1 = [interface([idx1, idx2]), idx(edge2elem(isEdge1, 1))];
        idx1 = edge2elem(isEdge2,2) + NT*(next(edge2elem(isEdge2, 4)) - 1);
        idx2 = edge2elem(isEdge2,2) + NT*(prev(edge2elem(isEdge2, 4)) - 1);
        edge2 = [interface([idx1, idx2]), idx(edge2elem(isEdge2, 2))];
        edge = [edge1;edge2];
        [~,J] = sort(edge(:, 3));
        edge = edge(J, [1, 2]);
        % Construct the polygon 
        NP = size(I,1)/numtri;
        NV = numtri + 2;
        polygon = zeros(NP,6);
        polygon(:, [1,2]) = edge(1:NV:end,:);
        for i = 2:NV-1
           tmp = repmat(polygon(:, i)', NV, 1);
           tmp = tmp(:);
           isNext = edge(:,1) == tmp;
           polygon(:, i+1) = edge(isNext, 2);
        end
    end

end