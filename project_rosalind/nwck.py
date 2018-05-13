
#!/usr/bin/env python

class Node(object):
    '''Node class for Newick Trees.'''
    def __init__(self, number, parent, name=None):
        '''Node initialization.'''
        self.number = number
        self.parent = parent
        self.children = []
        self.name = [name, 'Node_' + str(self.number)][name is None]

    def __repr__(self):
        '''Defines how Node instances are printed.'''
        return ['Node_' + str(self.number) + '('+str(self.name)+')', str(self.name)+'()'][self.name == 'Node_'+str(self.number)]

    def add_child(self, child):
    	'''Add a child to the node.'''
    	self.children.append(child)



class Newick(object):
    '''Creates a Newick Tree from the given data.'''
    def __init__(self, data):
        '''Initialize the Newick Tree.'''
        self.nodes = []
        self.node_index = 0
        self.edges = []
        self.construct_tree(data)
        self.name_index = {node.name: node.number for node in self.nodes}

    def construct_tree(self, data):
        '''Constructs the Newick Tree.'''
        data = data.replace(',', ' ').replace('(','( ').replace(')',' )').strip(';').split()
        current_parent = Node(-1, None)
        for item in data:
            if item[0] == '(':
                current_parent = Node(len(self.nodes), current_parent.number)
                self.nodes.append(current_parent)
                if len(self.nodes) > 1:
                    self.nodes[current_parent.parent].add_child(current_parent.number)
                    self.edges.append((current_parent.parent, current_parent.number))

            elif item[0] == ')':
                if len(item) > 1:
                    current_parent.name = item[1:]
                current_parent = self.nodes[current_parent.parent]

            else:
                self.nodes[current_parent.number].add_child(len(self.nodes))
                self.edges.append((current_parent.number, len(self.nodes)))
                self.nodes.append(Node(len(self.nodes), current_parent.number, item))

    def edge_names(self):
        '''Return a list of edges referencing node names.'''
        return [(self.nodes[edge[0]].name, self.nodes[edge[1]].name) for edge in self.edges]

    def distance(self, name1, name2):
        '''Returns the distance between name1 and name2.'''
        if name1 == name2:
            return 0

        # Create the branches from the two desired nodes to the root.
        branch1 = [self.name_index[name1]]
        branch2 = [self.name_index[name2]]
        while self.nodes[branch1[-1]].parent != -1:
            branch1.append(self.nodes[branch1[-1]].parent)
        while self.nodes[branch2[-1]].parent != -1:
            branch2.append(self.nodes[branch2[-1]].parent)

        return len(set(branch1) ^ set(branch2)) + 1

    def get_descendants(self, node_name):
        descendants = []
        for child in self.nodes[self.name_index[node_name]].children:
            descendants.append(self.nodes[child].name)
            descendants += self.get_descendants(self.nodes[child].name)

	return descendants


with open('/home/augusto/Downloads/rosalind_nwck.txt') as input_data:
	trees = [line.split('\n') for line in input_data.read().strip().split('\n\n')]

# The majority of the work is done by the Newick class in the Data Structures script.
distances = [str(Newick(tree[0]).distance(*tree[1].split())) for tree in trees]

for valor in distances:
	print int(valor) - 1
# Print and save the answer.
