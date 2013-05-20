
class Main
{
  int main()
  {
    System.srand();

    this.test(10,    1000);
    this.test(10,    1000);
    this.test(100,   1000);
    this.test(100,   1000);
    this.test(1000,  1000);
    this.test(1000,  1000);
    this.test(10000, 1000);
    this.test(10000, 1000);
  }

  LinkedList newList() { 
    LinkedList list;
    list = new LinkedList(new ListItem(null,null,null));
    list.clear();
    return list;
  }

  void test(int num_elems, int num_lists)
  {
    LinkedList list;
    list = this.newList();
    int hsize; hsize = System.heapsize();
    int i; i = 0;
    
    System.print("Test with "); System.print(num_lists);
    System.print(" lists with "); System.print(num_elems);
    System.print(" elements each.");
    System.print(" Initial heap size is: ");
    System.println(hsize);

    while(i < num_elems){
        list.clear();
        //System.gcollect();
        i=i+1;
        this.fillList(num_elems,list);
        int tmpsize; tmpsize = System.heapsize();
        if(tmpsize != hsize)
        {
           hsize = tmpsize;
           System.print("Heap size changed to: ");
           System.println(hsize);
        }
    }
  }


  void fillList(int num_elems, LinkedList list)
  {
    ListIterator it;
    it = list.head();
    int i; i = 0;
    while(i<num_elems)
    {
      i=i+1;
      list.insertAfter(new Integer(0),it);
      it.next();
    }
  }

}

class Integer {
  int v0;
}

class ListItem {
  Object data;
  ListItem previous;
  ListItem  next;

}

class ListIterator {
  LinkedList owner;
  ListItem pos;

  void head() { this.pos = this.owner.head; }
  void next() { this.pos = this.pos.next; }

  void previous() { this.pos = this.pos.previous; }

  bool hasMoreElements() {  
    return this.pos != this.owner.head;
  }

  Object nextElement() {
    if (this.pos != this.owner.head) {
       Object data; data  = this.pos.data;
       this.next();
       return data;
    }
    return null;
  }
}

class LinkedList {
  ListItem head;

  void clear() {
      this.head.next = this.head;
      this.head.previous = this.head;
  }

  bool isEmpty() { return this.head.next == this.head; }

  void insertAfter(Object data, ListIterator cursor) {
    ListItem newItem;
    newItem = new ListItem(data,cursor.pos,cursor.pos.next);
    newItem.next.previous = newItem;
    cursor.pos.next = newItem;
  }

  void insertBefore(Object data, ListIterator cursor) {
    ListItem newItem;
    newItem = new ListItem(data, cursor.pos.previous, cursor.pos);
    newItem.previous.next = newItem;
    cursor.pos.previous = newItem;
  }

  ListIterator head() {
    return new ListIterator(this, this.head);
  }
}
