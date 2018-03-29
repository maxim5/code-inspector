<?php if ( ! defined('BASEPATH')) exit('No direct script access allowed');

class Manage extends CI_Controller {
  public $item;
  public $act;
  public $userid;
  public $username;
  public $realname;

  function __construct() {
    parent::__construct();
    $user = $this->session->userdata('user');
    $this->userid = isset($user['id']) ? $user['id'] : 0;
    $this->username = isset($user['name']) ? $user['name'] : 0;
    $this->realname = isset($user['real']) ? $user['real'] : 0;
    $this->item = $this->uri->segment(2);
    //如果用户没有登录
    if($this->userid <= 0) {
      if($this->item != 'login' && $this->item != 'logout') {
        header("Location: /manage/login");
        exit;
      }
    }
    $this->act = $this->uri->segment(3);
    //载入manage config
    $this->config->load('manage');
    $navarray = $this->config->item('nav');
    $navitem = array();
    foreach($navarray as $k=>$v) {
      $navitem[$k] = $v['item'];
    }

    $this->_header();
  }

  /**
   * 根据地址生成菜单
   */
  private function _nav() {
    $nav = $this->config->item('nav');
    //开始按照seq排序处理
    $navlist = array();
    foreach($nav as $v) {
      $sub = $v['sub'];
      $sublist = array();
      foreach($sub as $vv) {
        $sublist[$vv['seq']] = $vv;
      }
      ksort($sublist);
      $v['sub'] = $sublist;
      $navlist[$v['seq']] = $v;
    }
    ksort($navlist);
    $out['nav']  = $navlist;
    $out['item'] = $this->item;
    $out['act']  = $this->act;
    return $this->load->view('manage/nav.html', $out, true);
  }

  /**
   * 根据菜单地址 生成头部并输出
   */
  private function _header() {
    if($this->item != 'login' && $this->item != 'logout') {
      //生成导航菜单
      $header['nav'] = $this->_nav();
      $header['user']['id'] = $this->userid;
      //输出头部
      $this->load->view('manage/header.html', $header);
    }
  }

  function index() { //后台管理默认页面
    $out = array();
    $out['user']['id']   = $this->userid;
    $out['user']['name'] = $this->username;
    $out['user']['real'] = $this->realname;
    $usercookie  = $this->input->cookie('user');
    $usercookie  = json_decode($usercookie, true);
    $out['user'] = array_merge($out['user'], $usercookie);
    $this->load->view('manage/index.html', $out);
    $this->load->view('manage/footer.html');
  }

  function login() { //后台管理登录
    if($this->input->is_post()) { //post
      $username = $this->input->post('username');
      $password = $this->input->post('password');
      $this->load->model('usermodel');
      $user = $this->usermodel->login($username, $password);
      $role = isset($user['role']) ? $user['role'] : 0;
      $res = array();
      $res['stat'] = 0;
      $res['data'] = array();
      if($role <= 0) { //登录失败
        $res['stat'] = 1;
      } else { //登录成功
        //userid username realname写入到session
        $usersess = array();
        $usersess['id'] = $user['id'];
        $usersess['name'] = $user['username'];
        $usersess['real'] = $user['realname'];
        $this->session->set_userdata(array('user' => $usersess));
        //role addtm lasttm lastip 写入到cookie
        $usercookie = array();
        $rolearray = $this->config->item('role');
        $usercookie['role'] = $rolearray[$role];
        $usercookie['addtm'] = date('Y-m-d H:i:s', $user['addtm']);
        $usercookie['lasttm'] = date('Y-m-d H:i:s', $user['lasttm']);
        $usercookie['lastip'] = long2ip($user['lastip']);
        $usercookie = json_encode($usercookie);
        $cookie = array(
          'name'   => 'user',
          'value'  => $usercookie,
          'expire' => 60*60*24*1000 //1000天过期时间
        );
        $this->input->set_cookie($cookie);
        $res['data'] = $user;
      }
      echo json_encode($res);
      exit;
    } else { //page
      $this->load->view('manage/header-login.html');
      $this->load->view('manage/login.html');
      $this->load->view('manage/footer-login.html');
    }
  }

  function logout() { //后台管理登出
    //清理session
    $this->session->unset_userdata('user');
    //清理cookie
    $cookie = array(
      'name'   => 'user',
      'value'  => '',
      'expire' => ''
    );
    $this->input->set_cookie($cookie);
    //跳转到登录页
    echo '<script>window.location.href="/manage/login";</script>';
    exit;
  }
  
  function course($act = '', $val = 0) { //后台课程管理
    $this->load->model('usermodel');
    switch($act) {
      case 'list':
        $out = array();
        $userlist = $this->usermodel->get();
        $userout = array();
        $rolearray = $this->config->item('role');
        foreach($userlist as $user) {
          $user['rolename'] = $rolearray[$user['role']];
          $user['lasttm'] = date('Y-m-d H:i:s', $user['lasttm']);
          $user['lastip'] = long2ip($user['lastip']);
          $userout[] = $user;
        }
        $out['user'] = $userout;
        $this->load->view('manage/course/list.html', $out);
        $this->load->view('manage/footer.html');
        break;
      case 'add':
        if($this->input->is_post()) { //post
          
        } else {
          $this->load->view('manage/course/add.html');
          $this->load->view('manage/footer.html');
        }
        break;
      case 'edit':
        if($this->input->is_post()) { //post
          
        } else {
          $this->load->view('manage/course/edit.html');
          $this->load->view('manage/footer.html');
        }
        break;
      case 'info':
        $user = $this->usermodel->get("`id` = '$val'");
        if(isset($user[0])) {
          $user = $user[0];
          $rolearray = $this->config->item('role');
          $user['rolename'] = $rolearray[$user['role']];
          $user['addtm'] = date('Y-m-d H:i:s', $user['addtm']);
          $user['lasttm'] = date('Y-m-d H:i:s', $user['lasttm']);
          $user['lastip'] = long2ip($user['lastip']);
          $out = array();
          $out['user'] = $user;
          $this->load->view('manage/course/info.html', $out);
          $this->load->view('manage/footer.html');
        } else {
          show_404();
        }
        break;
      default:
        show_404();
        break;
    }
  }

  function user($act = '', $val = 0) { //后台用户管理
    $this->load->model('usermodel');
    switch($act) {
      case 'list':
        $out = array();
        $userlist = $this->usermodel->get();
        $userout = array();
        $rolearray = $this->config->item('role');
        foreach($userlist as $user) {
          $user['rolename'] = $rolearray[$user['role']];
          $user['lasttm'] = date('Y-m-d H:i:s', $user['lasttm']);
          $user['lastip'] = long2ip($user['lastip']);
          $userout[] = $user;
        }
        $out['user'] = $userout;
        $this->load->view('manage/user/list.html', $out);
        $this->load->view('manage/footer.html');
        break;
      case 'add':
        if($this->input->is_post()) { //post
          
        } else {
          $this->load->view('manage/user/add.html');
          $this->load->view('manage/footer.html');
        }
        break;
      case 'edit':
        if($this->input->is_post()) { //post
          
        } else {
          $this->load->view('manage/user/edit.html');
          $this->load->view('manage/footer.html');
        }
        break;
      case 'info':
        $user = $this->usermodel->get("`id` = '$val'");
        if(isset($user[0])) {
          $user = $user[0];
          $rolearray = $this->config->item('role');
          $user['rolename'] = $rolearray[$user['role']];
          $user['addtm'] = date('Y-m-d H:i:s', $user['addtm']);
          $user['lasttm'] = date('Y-m-d H:i:s', $user['lasttm']);
          $user['lastip'] = long2ip($user['lastip']);
          $out = array();
          $out['user'] = $user;
          $this->load->view('manage/user/info.html', $out);
          $this->load->view('manage/footer.html');
        } else {
          show_404();
        }
        break;
      default:
        show_404();
        break;
    }
  }
}