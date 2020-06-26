/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.StoreServices;
import eu.internetofus.common.components.profile_manager.WeNetUserProfile;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.service.TaskConcludedNotification;
import eu.internetofus.common.components.service.TaskConcludedNotification.Outcome;
import eu.internetofus.common.components.service.TaskProposalNotification;
import eu.internetofus.common.components.service.TaskSelectionNotification;
import eu.internetofus.common.components.service.TaskVolunteerNotification;
import eu.internetofus.common.components.service.TextualMessage;
import eu.internetofus.common.components.service.WeNetServiceSimulator;
import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.TaskGoal;
import eu.internetofus.common.components.task_manager.TaskTransaction;
import eu.internetofus.common.components.task_manager.TaskType;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxTestContext;

/**
 * Integration test over the hard coded protocol. ATTENTION: This test is sequential and maintains the state between
 * methods. In other words, you must to run the entire test methods on the specified order to work.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
@TestMethodOrder(OrderAnnotation.class)
public class HardcodedProtocolIT {

  /**
   * -number maximum of users to use on the test.
   */
  private static final int MAX_USERS = 6;

  /**
   * The users that will involved on the test.
   */
  protected static List<WeNetUserProfile> users;

  /**
   * The application that will involved on the test.
   */
  protected static App app;

  /**
   * The task type that will involved on the test.
   */
  protected static TaskType taskType;

  /**
   * The task that will involved on the test.
   */
  protected static Task task;

  /**
   * Create user for the test.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   *
   * @return the future that will return the created users.
   */
  protected Future<List<WeNetUserProfile>> createUsers(final Vertx vertx, final VertxTestContext testContext) {

    final Promise<List<WeNetUserProfile>> promise = Promise.promise();
    Future<List<WeNetUserProfile>> future = promise.future();
    for (int i = 0; i < MAX_USERS; i++) {

      future = future.compose(users -> {

        final Promise<List<WeNetUserProfile>> profilePromise = Promise.promise();
        StoreServices.storeProfile(new WeNetUserProfile(), vertx, testContext, testContext.succeeding(profile -> {
          users.add(profile);
          profilePromise.complete(users);
        }));
        return profilePromise.future();

      });

    }

    promise.complete(new ArrayList<>());

    return future;
  }

  /**
   * Create the APP for the test.
   *
   * @param users       to be defined on the App.
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   *
   * @return the future that will return the created users.
   */
  protected Future<App> createApp(final List<WeNetUserProfile> users, final Vertx vertx, final VertxTestContext testContext) {

    final Promise<App> promise = Promise.promise();
    StoreServices.storeApp(new App(), vertx, testContext, testContext.succeeding(app -> {

      final JsonArray appUsers = new JsonArray();
      for (final WeNetUserProfile profile : users) {

        appUsers.add(profile.id);
      }
      WeNetServiceSimulator.createProxy(vertx).addUsers(app.appId, appUsers, testContext.succeeding(added -> promise.complete(app)));
    }));

    return promise.future();
  }

  /**
   * Wait the App has received the specified call backs.
   *
   * @param appId          identifier of the application to get the call backs.
   * @param checkCallbacks the function that has to be true to finish the get the callbacks.
   * @param vertx          event bus to use.
   * @param testContext    context to do the test.
   *
   * @return the future that will return the callbacks that satisfy the predicate.
   */
  protected Future<JsonArray> waitUntilCallbacks(final String appId, final Predicate<JsonArray> checkCallbacks, final Vertx vertx, final VertxTestContext testContext) {

    final Promise<JsonArray> promise = Promise.promise();
    WeNetServiceSimulator.createProxy(vertx).retrieveJsonCallbacks(appId, testContext.succeeding(callbacks -> {

      if (checkCallbacks.test(callbacks)) {

        promise.complete(callbacks);

      } else {

        this.waitUntilCallbacks(appId, checkCallbacks, vertx, testContext).onComplete(testContext.succeeding(result -> promise.complete(result)));
      }

    }));

    return promise.future();
  }

  /**
   * Wait until the task satisfy a predicate.
   *
   * @param taskId      identifier of the task to get the information.
   * @param checkTask   the function that has to be true to the task is on the state that is waiting.
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   *
   * @return the future that will return the task that satisfy the predicate.
   */
  protected Future<Task> waitUntilTask(final String taskId, final Predicate<Task> checkTask, final Vertx vertx, final VertxTestContext testContext) {

    final Promise<Task> promise = Promise.promise();
    WeNetTaskManager.createProxy(vertx).retrieveTask(taskId, testContext.succeeding(task -> {

      if (checkTask.test(task)) {

        promise.complete(task);

      } else {

        this.waitUntilTask(taskId, checkTask, vertx, testContext).onComplete(testContext.succeeding(result -> promise.complete(result)));
      }

    }));

    return promise.future();
  }

  /**
   * Create the users that will be used on the tests.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(1)
  public void shouldCreateUsers(final Vertx vertx, final VertxTestContext testContext) {

    this.createUsers(vertx, testContext).onComplete(testContext.succeeding(users -> {
      HardcodedProtocolIT.users = users;
      testContext.completeNow();
    }));
  }

  /**
   * Create the app that will be used on the tests.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(2)
  public void shouldCreateApp(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.users != null;
    this.createApp(HardcodedProtocolIT.users, vertx, testContext).onComplete(testContext.succeeding(app -> {
      HardcodedProtocolIT.app = app;
      testContext.completeNow();
    }));
  }

  /**
   * Check that a task is created.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(3)
  public void shouldCreateTask(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.users != null;
    assert HardcodedProtocolIT.app != null;
    StoreServices.storeTaskTypeExample(1, vertx, testContext, testContext.succeeding(taskType -> {

      HardcodedProtocolIT.taskType = taskType;
      final Task task = new Task();
      task.appId = HardcodedProtocolIT.app.appId;
      task.deadlineTs = TimeManager.now() + 120;
      task.startTs = task.deadlineTs + 30;
      task.endTs = task.startTs + 300;
      task.taskTypeId = taskType.id;
      task.goal = new TaskGoal();
      task.goal.name = "Test create task";
      task.requesterId = HardcodedProtocolIT.users.get(0).id;
      WeNetTaskManager.createProxy(vertx).createTask(task, testContext.succeeding(createdTask -> {

        HardcodedProtocolIT.task = createdTask;
        this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

          final Set<String> ids = new HashSet<>();
          for (final WeNetUserProfile profile : HardcodedProtocolIT.users) {

            ids.add(profile.id);
          }
          ids.remove(createdTask.requesterId);

          for (int i = 0; i < callbacks.size(); i++) {

            final TaskProposalNotification proposal = Model.fromJsonObject(callbacks.getJsonObject(i), TaskProposalNotification.class);
            if (proposal != null && createdTask.id.equals(proposal.taskId)) {

              ids.remove(proposal.recipientId);

            }
          }
          return ids.isEmpty();

        }, vertx, testContext).onComplete(testContext.succeeding(msg -> {
          WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
            WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

              assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
              final JsonObject attributes = storedTask.attributes;
              final JsonArray unanswered = attributes.getJsonArray("unanswered");
              assertThat(unanswered).isNotNull().doesNotContain(createdTask.requesterId);
              for (final WeNetUserProfile profile : HardcodedProtocolIT.users) {

                if (!profile.id.equals(storedTask.requesterId)) {

                  assertThat(unanswered).contains(profile.id);

                }
              }
              HardcodedProtocolIT.task = storedTask;
              testContext.completeNow();
            })));
          }));
        }));

      }));
    }));
  }

  /**
   * Check that an user decline to do a task.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(4)
  public void shouldDeclineTask(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.task != null;
    assert HardcodedProtocolIT.users != null;

    final TaskTransaction taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedProtocolIT.task.id;
    taskTransaction.label = "refuseTask";
    final String volunteerId = HardcodedProtocolIT.users.get(1).id;
    taskTransaction.attributes = new JsonObject().put("volunteerId", volunteerId);
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction, testContext.succeeding(done -> {

      this.waitUntilTask(HardcodedProtocolIT.task.id, task -> {

        return task.attributes instanceof JsonObject && task.attributes.getJsonArray("declined", new JsonArray()).contains(volunteerId);

      }, vertx, testContext).onComplete(testContext.succeeding(task -> testContext.verify(() -> {

        assertThat(task.attributes).isNotNull().isInstanceOf(JsonObject.class);
        final JsonObject attributes = task.attributes;
        final JsonArray unanswered = attributes.getJsonArray("unanswered");
        assertThat(unanswered).isNotNull().doesNotContain(task.requesterId, volunteerId);
        final JsonArray declined = attributes.getJsonArray("declined");
        assertThat(declined).isNotNull().hasSize(1).contains(volunteerId);
        testContext.completeNow();

      })));

    }));

  }

  /**
   * Check that an user can not be a volunteer if it has declined.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(5)
  public void shouldNotVolunteerForTaskAfterDecline(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.task != null;
    assert HardcodedProtocolIT.users != null;

    final TaskTransaction taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedProtocolIT.task.id;
    taskTransaction.label = "volunteerForTask";
    final String volunteerId = HardcodedProtocolIT.users.get(1).id;
    taskTransaction.attributes = new JsonObject().put("volunteerId", volunteerId);
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction, testContext.succeeding(done -> {

      this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

        for (int i = 0; i < callbacks.size(); i++) {

          final TextualMessage message = Model.fromJsonObject(callbacks.getJsonObject(i), TextualMessage.class);
          if (message != null && volunteerId.equals(message.recipientId)) {

            return true;

          }
        }

        return false;

      }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
          WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

            assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
            final JsonObject attributes = storedTask.attributes;
            final JsonArray unanswered = attributes.getJsonArray("unanswered");
            assertThat(unanswered).isNotNull().doesNotContain(storedTask.requesterId, volunteerId);
            final JsonArray declined = attributes.getJsonArray("declined");
            assertThat(declined).isNotNull().hasSize(1).contains(volunteerId);
            HardcodedProtocolIT.task = storedTask;
            testContext.completeNow();

          })));
        }));
      }));
    }));

  }

  /**
   * Check that an user is accept to be volunteer of a task.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(6)
  public void shouldVolunteerForTask(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.task != null;
    assert HardcodedProtocolIT.users != null;

    Future<Void> future = Future.succeededFuture();
    for (int i = 2; i < MAX_USERS - 1; i++) {

      final String volunteerId = HardcodedProtocolIT.users.get(i).id;
      future = future.compose(map -> this.doTaskTransactionVolunteerForTask(volunteerId, vertx, testContext));
    }

    future.onComplete(testContext.succeeding(empty -> testContext.verify(() -> {

      final JsonArray volunteers = HardcodedProtocolIT.task.attributes.getJsonArray("volunteers");
      assertThat(volunteers).isNotNull().hasSize(MAX_USERS - 3);
      testContext.completeNow();

    })));

  }

  /**
   * Realize a task transaction to be a volunteer for the task
   *
   * @param volunteerId identifier of the user that has to be a volunteer.
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   *
   * @return a future that inform if the task transaction to be a volunteer success or not.
   */
  protected Future<Void> doTaskTransactionVolunteerForTask(final String volunteerId, final Vertx vertx, final VertxTestContext testContext) {

    final Promise<Void> promise = Promise.promise();
    final TaskTransaction taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedProtocolIT.task.id;
    taskTransaction.label = "volunteerForTask";
    taskTransaction.attributes = new JsonObject().put("volunteerId", volunteerId);
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction, testContext.succeeding(done -> {

      this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

        for (int i = 0; i < callbacks.size(); i++) {

          final TaskVolunteerNotification notification = Model.fromJsonObject(callbacks.getJsonObject(i), TaskVolunteerNotification.class);
          if (notification != null && notification.volunteerId != null && notification.volunteerId.equals(volunteerId) && HardcodedProtocolIT.task.requesterId.equals(notification.recipientId)) {

            return true;

          }
        }

        return false;

      }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
          WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

            assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
            final JsonObject attributes = storedTask.attributes;
            final JsonArray unanswered = attributes.getJsonArray("unanswered");
            assertThat(unanswered).isNotNull().doesNotContain(task.requesterId, volunteerId);
            final JsonArray volunteers = attributes.getJsonArray("volunteers");
            assertThat(volunteers).isNotNull().contains(volunteerId);
            HardcodedProtocolIT.task = storedTask;
            promise.complete();

          })));
        }));
      }));
    }));

    return promise.future();

  }

  /**
   * Check that an user can not decline a task if it accepted to be volunteer.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(7)
  public void shouldNotDeclineAfterVolunteerForTask(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.task != null;
    assert HardcodedProtocolIT.users != null;

    final TaskTransaction taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedProtocolIT.task.id;
    taskTransaction.label = "refuseTask";
    final String volunteerId = HardcodedProtocolIT.users.get(2).id;
    taskTransaction.attributes = new JsonObject().put("volunteerId", volunteerId);
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction, testContext.succeeding(done -> {

      this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

        for (int i = 0; i < callbacks.size(); i++) {

          final TextualMessage message = Model.fromJsonObject(callbacks.getJsonObject(i), TextualMessage.class);
          if (message != null && volunteerId.equals(message.recipientId)) {

            return true;

          }
        }

        return false;

      }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
          WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

            assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
            final JsonObject attributes = storedTask.attributes;
            final JsonArray unanswered = attributes.getJsonArray("unanswered");
            assertThat(unanswered).isNotNull().doesNotContain(storedTask.requesterId, volunteerId);
            final JsonArray volunteers = attributes.getJsonArray("volunteers");
            assertThat(volunteers).isNotNull().contains(volunteerId);
            HardcodedProtocolIT.task = storedTask;
            testContext.completeNow();

          })));
        }));
      }));
    }));

  }

  /**
   * Check that an user can be accepted to be volunteer.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(8)
  public void shouldAcceptVolunteer(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.task != null;
    assert HardcodedProtocolIT.users != null;

    final TaskTransaction taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedProtocolIT.task.id;
    taskTransaction.label = "acceptVolunteer";
    final String volunteerId = HardcodedProtocolIT.users.get(2).id;
    taskTransaction.attributes = new JsonObject().put("volunteerId", volunteerId);
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction, testContext.succeeding(done -> {

      this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

        for (int i = 0; i < callbacks.size(); i++) {

          final TaskSelectionNotification notification = Model.fromJsonObject(callbacks.getJsonObject(i), TaskSelectionNotification.class);
          if (notification != null && volunteerId.equals(notification.recipientId)) {

            return true;

          }
        }

        return false;

      }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
          WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

            assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
            final JsonObject attributes = storedTask.attributes;
            final JsonArray volunteers = attributes.getJsonArray("volunteers");
            assertThat(volunteers).isNotNull().doesNotContain(storedTask.requesterId, volunteerId);
            final JsonArray accepted = attributes.getJsonArray("accepted");
            assertThat(accepted).isNotNull().hasSize(1).contains(volunteerId);
            HardcodedProtocolIT.task = storedTask;
            testContext.completeNow();

          })));
        }));
      }));
    }));

  }

  /**
   * Check that an user can not be accepted as volunteer.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(9)
  public void shouldNotAcceptVolunteer(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.task != null;
    assert HardcodedProtocolIT.users != null;

    final TaskTransaction taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedProtocolIT.task.id;
    taskTransaction.label = "acceptVolunteer";
    final String volunteerId = HardcodedProtocolIT.users.get(1).id;
    taskTransaction.attributes = new JsonObject().put("volunteerId", volunteerId);
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction, testContext.succeeding(done -> {

      this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

        for (int i = 0; i < callbacks.size(); i++) {

          final TextualMessage message = Model.fromJsonObject(callbacks.getJsonObject(i), TextualMessage.class);
          if (message != null && HardcodedProtocolIT.task.requesterId.equals(message.recipientId)) {

            return true;

          }
        }

        return false;

      }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
          WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

            assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
            final JsonObject attributes = storedTask.attributes;
            final JsonArray volunteers = attributes.getJsonArray("volunteers");
            assertThat(volunteers).isNotNull().doesNotContain(storedTask.requesterId, volunteerId);
            final JsonArray accepted = attributes.getJsonArray("accepted");
            assertThat(accepted).isNotNull().doesNotContain(volunteerId);
            HardcodedProtocolIT.task = storedTask;
            testContext.completeNow();

          })));
        }));
      }));
    }));

  }

  /**
   * Check that an user can be refused to be volunteer.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(10)
  public void shouldRefuseVolunteer(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.task != null;
    assert HardcodedProtocolIT.users != null;

    final TaskTransaction taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedProtocolIT.task.id;
    taskTransaction.label = "refuseVolunteer";
    final String volunteerId = HardcodedProtocolIT.users.get(3).id;
    taskTransaction.attributes = new JsonObject().put("volunteerId", volunteerId);
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction, testContext.succeeding(done -> {

      this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

        for (int i = 0; i < callbacks.size(); i++) {

          final TaskSelectionNotification notification = Model.fromJsonObject(callbacks.getJsonObject(i), TaskSelectionNotification.class);
          if (notification != null && volunteerId.equals(notification.recipientId)) {

            return true;

          }
        }

        return false;

      }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
          WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

            assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
            final JsonObject attributes = storedTask.attributes;
            final JsonArray volunteers = attributes.getJsonArray("volunteers");
            assertThat(volunteers).isNotNull().doesNotContain(storedTask.requesterId, volunteerId);
            final JsonArray refused = attributes.getJsonArray("refused");
            assertThat(refused).isNotNull().hasSize(1).contains(volunteerId);
            HardcodedProtocolIT.task = storedTask;
            testContext.completeNow();

          })));
        }));
      }));
    }));

  }

  /**
   * Check that an user can not be refused as volunteer.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(11)
  public void shouldNotRefuseVolunteer(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.task != null;
    assert HardcodedProtocolIT.users != null;

    final TaskTransaction taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedProtocolIT.task.id;
    taskTransaction.label = "refuseVolunteer";
    final String volunteerId = HardcodedProtocolIT.users.get(1).id;
    taskTransaction.attributes = new JsonObject().put("volunteerId", volunteerId);
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction, testContext.succeeding(done -> {

      this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

        for (int i = 0; i < callbacks.size(); i++) {

          final TextualMessage message = Model.fromJsonObject(callbacks.getJsonObject(i), TextualMessage.class);
          if (message != null && HardcodedProtocolIT.task.requesterId.equals(message.recipientId)) {

            return true;

          }
        }

        return false;

      }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
          WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

            assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
            final JsonObject attributes = storedTask.attributes;
            final JsonArray volunteers = attributes.getJsonArray("volunteers");
            assertThat(volunteers).isNotNull().doesNotContain(storedTask.requesterId, volunteerId);
            final JsonArray refused = attributes.getJsonArray("refused");
            assertThat(refused).isNotNull().doesNotContain(volunteerId);
            HardcodedProtocolIT.task = storedTask;
            testContext.completeNow();

          })));
        }));
      }));
    }));

  }

  /**
   * Check that an user can mark the task as completed.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(12)
  public void shouldTaskCompleted(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.task != null;
    assert HardcodedProtocolIT.users != null;

    final TaskTransaction taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedProtocolIT.task.id;
    taskTransaction.label = "taskCompleted";
    final Outcome outcome = TaskConcludedNotification.Outcome.completed;
    taskTransaction.attributes = new JsonObject().put("outcome", outcome);
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction, testContext.succeeding(done -> {

      this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

        final Set<String> ids = new HashSet<>();
        ids.add(HardcodedProtocolIT.users.get(2).id);
        ids.add(HardcodedProtocolIT.users.get(4).id);
        ids.add(HardcodedProtocolIT.users.get(5).id);

        for (int i = 0; i < callbacks.size(); i++) {

          final TaskConcludedNotification notification = Model.fromJsonObject(callbacks.getJsonObject(i), TaskConcludedNotification.class);
          if (notification != null && notification.outcome == outcome) {

            ids.remove(notification.recipientId);

          }
        }
        return ids.isEmpty();

      }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
          WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

            assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
            final JsonObject attributes = storedTask.attributes;
            assertThat(attributes.getString("outcome")).isNotNull().isEqualTo(outcome.name());
            final JsonArray declined = attributes.getJsonArray("declined");
            assertThat(declined).isNotNull().hasSize(1).contains(users.get(1).id);
            final JsonArray accepted = attributes.getJsonArray("accepted");
            assertThat(accepted).isNotNull().hasSize(1).contains(users.get(2).id);
            final JsonArray refused = attributes.getJsonArray("refused");
            assertThat(refused).isNotNull().hasSize(1).contains(users.get(3).id);
            final JsonArray volunteers = attributes.getJsonArray("volunteers");
            assertThat(volunteers).isNotNull().hasSize(1).contains(users.get(4).id);
            final JsonArray unanswered = attributes.getJsonArray("unanswered");
            assertThat(unanswered).isNotNull().hasSize(1).contains(users.get(5).id);
            HardcodedProtocolIT.task = storedTask;
            testContext.completeNow();

          })));
        }));
      }));
    }));

  }

  /**
   * Check that an user can not complete a closed task.
   *
   * @param outcome     that can not be set to a closed task.
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @ParameterizedTest(name = "Should not complete a task with the outcome {0}")
  @EnumSource(TaskConcludedNotification.Outcome.class)
  @Order(13)
  public void shouldNotChangeCompletedAClosedTask(final TaskConcludedNotification.Outcome outcome, final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.task != null;
    assert HardcodedProtocolIT.users != null;

    final TaskTransaction taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedProtocolIT.task.id;
    taskTransaction.label = "taskCompleted";
    taskTransaction.attributes = new JsonObject().put("outcome", outcome);
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction, testContext.succeeding(done -> {

      this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

        for (int i = 0; i < callbacks.size(); i++) {

          final TextualMessage message = Model.fromJsonObject(callbacks.getJsonObject(i), TextualMessage.class);
          if (message != null && HardcodedProtocolIT.task.requesterId.equals(message.recipientId)) {

            return true;

          }
        }
        return false;

      }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
          WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

            assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
            final JsonObject attributes = storedTask.attributes;
            assertThat(attributes.getString("outcome")).isNotNull().isEqualTo(TaskConcludedNotification.Outcome.completed.name());
            final JsonArray declined = attributes.getJsonArray("declined");
            assertThat(declined).isNotNull().hasSize(1).contains(users.get(1).id);
            final JsonArray accepted = attributes.getJsonArray("accepted");
            assertThat(accepted).isNotNull().hasSize(1).contains(users.get(2).id);
            final JsonArray refused = attributes.getJsonArray("refused");
            assertThat(refused).isNotNull().hasSize(1).contains(users.get(3).id);
            final JsonArray volunteers = attributes.getJsonArray("volunteers");
            assertThat(volunteers).isNotNull().hasSize(1).contains(users.get(4).id);
            final JsonArray unanswered = attributes.getJsonArray("unanswered");
            assertThat(unanswered).isNotNull().hasSize(1).contains(users.get(5).id);
            HardcodedProtocolIT.task = storedTask;
            testContext.completeNow();

          })));
        }));
      }));
    }));

  }

  /**
   * Check that can not modify the volunteers of a closed task.
   *
   * @param label       of the task transaction that can not be done.
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @ParameterizedTest(name = "Should not do the task transaction {0}")
  @ValueSource(strings = { "volunteerForTask", "refuseTask", "acceptVolunteer", "refuseVolunteer" })
  @Order(14)
  public void shouldNotChangeVolunteerStatesWhenTaskIsClosed(final String label, final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.task != null;
    assert HardcodedProtocolIT.users != null;

    final TaskTransaction taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedProtocolIT.task.id;
    taskTransaction.label = label;
    final String volunteerId = HardcodedProtocolIT.users.get(5).id;
    taskTransaction.attributes = new JsonObject().put("volunteerId", volunteerId);
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction, testContext.succeeding(done -> {

      this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

        for (int i = 0; i < callbacks.size(); i++) {

          final TextualMessage message = Model.fromJsonObject(callbacks.getJsonObject(i), TextualMessage.class);
          if (message != null && volunteerId.equals(message.recipientId)) {

            return true;

          }
        }
        return false;

      }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
          WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

            assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
            final JsonObject attributes = storedTask.attributes;
            assertThat(attributes.getString("outcome")).isNotNull().isEqualTo(TaskConcludedNotification.Outcome.completed.name());
            final JsonArray declined = attributes.getJsonArray("declined");
            assertThat(declined).isNotNull().hasSize(1).contains(users.get(1).id);
            final JsonArray accepted = attributes.getJsonArray("accepted");
            assertThat(accepted).isNotNull().hasSize(1).contains(users.get(2).id);
            final JsonArray refused = attributes.getJsonArray("refused");
            assertThat(refused).isNotNull().hasSize(1).contains(users.get(3).id);
            final JsonArray volunteers = attributes.getJsonArray("volunteers");
            assertThat(volunteers).isNotNull().hasSize(1).contains(users.get(4).id);
            final JsonArray unanswered = attributes.getJsonArray("unanswered");
            assertThat(unanswered).isNotNull().hasSize(1).contains(users.get(5).id);
            HardcodedProtocolIT.task = storedTask;
            testContext.completeNow();

          })));
        }));
      }));
    }));

  }

  /**
   * Check that a task with a short deadline is created.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(15)
  public void shouldCreateTaskWithShortDeadline(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.users != null;
    assert HardcodedProtocolIT.app != null;
    StoreServices.storeTaskTypeExample(1, vertx, testContext, testContext.succeeding(taskType -> {

      HardcodedProtocolIT.taskType = taskType;
      final Task task = new Task();
      task.appId = HardcodedProtocolIT.app.appId;
      task.deadlineTs = TimeManager.now() + 10;
      task.startTs = task.deadlineTs + 30;
      task.endTs = task.startTs + 300;
      task.taskTypeId = taskType.id;
      task.goal = new TaskGoal();
      task.goal.name = "Test create task";
      task.requesterId = HardcodedProtocolIT.users.get(5).id;
      WeNetTaskManager.createProxy(vertx).createTask(task, testContext.succeeding(createdTask -> {

        HardcodedProtocolIT.task = createdTask;
        this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

          final Set<String> ids = new HashSet<>();
          for (final WeNetUserProfile profile : HardcodedProtocolIT.users) {

            ids.add(profile.id);
          }
          ids.remove(createdTask.requesterId);

          for (int i = 0; i < callbacks.size(); i++) {

            final TaskProposalNotification proposal = Model.fromJsonObject(callbacks.getJsonObject(i), TaskProposalNotification.class);
            if (proposal != null && createdTask.id.equals(proposal.taskId)) {

              ids.remove(proposal.recipientId);

            }
          }
          return ids.isEmpty();

        }, vertx, testContext).onComplete(testContext.succeeding(msg -> {
          WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
            WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

              assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
              final JsonObject attributes = storedTask.attributes;
              final JsonArray unanswered = attributes.getJsonArray("unanswered");
              assertThat(unanswered).isNotNull().doesNotContain(createdTask.requesterId);
              for (final WeNetUserProfile profile : HardcodedProtocolIT.users) {

                if (!profile.id.equals(storedTask.requesterId)) {

                  assertThat(unanswered).contains(profile.id);

                }
              }
              HardcodedProtocolIT.task = storedTask;
              testContext.completeNow();
            })));
          }));
        }));

      }));
    }));
  }

  /**
   * Check that not do a transaction if the deadline has reached.
   *
   * @param label       of the task transaction that can not be done after deadline has reached.
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @ParameterizedTest(name = "Should not do the task transaction {0} because deadline is reached")
  @ValueSource(strings = { "volunteerForTask", "refuseTask" })
  @Order(16)
  public void shouldNotDoTranasctionAfterDeadline(final String label, final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.users != null;
    assert HardcodedProtocolIT.task != null;

    while (TimeManager.now() <= HardcodedProtocolIT.task.deadlineTs ) {
      // Wait until deadline is reached
    }

    final TaskTransaction taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedProtocolIT.task.id;
    taskTransaction.label = label;
    final String volunteerId = HardcodedProtocolIT.users.get(0).id;
    taskTransaction.attributes = new JsonObject().put("volunteerId", volunteerId);
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction, testContext.succeeding(done -> {

      this.waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

        for (int i = 0; i < callbacks.size(); i++) {

          final TextualMessage message = Model.fromJsonObject(callbacks.getJsonObject(i), TextualMessage.class);
          if (message != null && volunteerId.equals(message.recipientId)) {

            return true;

          }
        }
        return false;

      }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> testContext.completeNow()));
      }));

    }));
  }
}
